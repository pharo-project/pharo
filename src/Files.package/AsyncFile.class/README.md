An asynchronous file allows simple file read and write operations to be performed in parallel with other processing. This is useful in multimedia applications that need to stream large amounts of sound or image data from or to a file while doing other work.

Closing the file after its use is currently required to not leak external semaphores. 
