Program allows gathering statistics from .csv files and presenting them in aggregated format.
 
Program uses FS2 library in order to stream through the files. Because of the streaming, it is capable of running
with small memory footprint. While running the tests, I have successfully streamed through ~1 GB files while limiting the 
heap size of the JVM to 32 MB.

In order to run the program, one has to execute command `sbt "run --path <path to csv files>"`