This is a simple project showing how to use [gperftools heapcheck](https://gperftools.github.io/gperftools/heap_checker.html)
in order to find off-heap memory leaks in Haskell programs.

# The Program

The test program allocates memory off-heap in two different ways. Firstly by
directly calling `mallocBytes` and secondly, indirectly by calling a library
function which happens to itself call `mallocBytes`. We'll use the gperftools
heapchecker in order to find where in our program these leaks arise from.

```
module Main where

import Foreign.Marshal.Alloc
import System.Log.FastLogger

main :: IO ()
main = do
  -- Direct leak
  mallocBytes 1024
  -- Leak from a library
  newFastLogger (LogStdout 1024)
  return ()
```

# Compiling

The executable just needs to be linked against `tcmalloc`, this is achieved by
adding `tcmalloc` to the `extra-libraries` field in the cabal file.

```
extra-libraries: tcmalloc
```

# Running

The program is then run with the `HEAPCHECK=normal` environment variable set.

```
PPROF_PATH=$(which pprof) HEAPCHECK=normal cabal new-run gperftools-example
```

Then when the program exits, there is a report about leaked memory:

```
[nix-shell:~/gperftools-example]$ ./run-example
Resolving dependencies...
Build profile: -w ghc-8.10.2 -O1
In order, the following will be built (use -v for more details):
 - gperftools-example-0.1.0.0 (exe:gperftools-example) (configuration changed)
Configuring executable 'gperftools-example' for gperftools-example-0.1.0.0..
Warning: The 'license-file' field refers to the file 'LICENSE' which does not
exist.
Preprocessing executable 'gperftools-example' for gperftools-example-0.1.0.0..
Building executable 'gperftools-example' for gperftools-example-0.1.0.0..
Linking /home/matt/gperftools-example/dist-newstyle/build/x86_64-linux/ghc-8.10.2/gperftools-example-0.1.0.0/x/gperftools-example/build/gperftools-example/gperftools-example ...
WARNING: Perftools heap leak checker is active -- Performance may suffer
Have memory regions w/o callers: might report false leaks
Leak check _main_ detected leaks of 9216 bytes in 9 objects
The 2 largest leaks:
Using local file /home/matt/gperftools-example/dist-newstyle/build/x86_64-linux/ghc-8.10.2/gperftools-example-0.1.0.0/x/gperftools-example/build/gperftools-example/gperftools-example.
Argument "MSWin32" isn't numeric in numeric eq (==) at /nix/store/j6mqm3xhmaq7m6js9gbvw7x3f5f1i32d-gperftools-2.8/bin/pprof line 5047.
Argument "linux" isn't numeric in numeric eq (==) at /nix/store/j6mqm3xhmaq7m6js9gbvw7x3f5f1i32d-gperftools-2.8/bin/pprof line 5047.
Leak of 8192 bytes in 8 objects allocated from:
	@ 418fb6 fastzmloggerzm3zi0zi3zmc8e1f84d3515a933118ea8b8e3bf49c760d60247807fe546889be0637e168844_SystemziLogziFastLoggerziLoggerSet_zdwnewFDLoggerSet_slow
Leak of 1024 bytes in 1 objects allocated from:
	@ 40ef29 Main_main1_info


If the preceding stack traces are not enough to find the leaks, try running THIS shell command:

pprof /home/matt/gperftools-example/dist-newstyle/build/x86_64-linux/ghc-8.10.2/gperftools-example-0.1.0.0/x/gperftools-example/build/gperftools-example/gperftools-example "/tmp/gperftools-example.10669._main_-end.heap" --inuse_objects --lines --heapcheck  --edgefraction=1e-10 --nodefraction=1e-10 --gv

If you are still puzzled about why the leaks are there, try rerunning this program with HEAP_CHECK_TEST_POINTER_ALIGNMENT=1 and/or with HEAP_CHECK_MAX_POINTER_OFFSET=-1
If the leak report occurs
Exiting with error code (instead of crashing) because of whole-program memory leaks
```

The important part of the program tells us there are two leaks:

```
Leak of 8192 bytes in 8 objects allocated from:
	@ 418fb6 fastzmloggerzm3zi0zi3zmc8e1f84d3515a933118ea8b8e3bf49c760d60247807fe546889be0637e168844_SystemziLogziFastLoggerziLoggerSet_zdwnewFDLoggerSet_slow
Leak of 1024 bytes in 1 objects allocated from:
	@ 40ef29 Main_main1_info
```

The first one of `1024` bytes comes from our explicit call to `mallocBytes` in `Main.hs`.

The second leak of 8192 bytes comes from internally in the `fast-logger` library. By decipering the
address we can see the symbol which causes the allocation is `newFDLoggerSet`, then
having a bit of a poke around you can find the call to `mallocBytes` which was inlined
into this definition is from `getBuffer`.

```
getBuffer :: BufSize -> IO Buffer
getBuffer = mallocBytes
```


