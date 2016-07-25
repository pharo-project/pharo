OSSPipe represents a pipe provided by the underlying operating system, such as a Unix pipe. I have a reader stream and a writer stream which behave similarly to a read-only FileStream and a writeable FileStream. These streams are instances of OSSAttachableFileStream which are attached  to the underlying created  pipe (to either read and write end). 

The idea of OSSPipe is to provide an Stream-like API for pipes. The write-related methods will be delagated to the 'writer' (for example, #nextPutAll:) and the read-related methods (like #upToEnd) will be forwarded to the reader.

Thanks to the Stream-API, it almos allows a code user, to either use Pipes or regular files polymorphically.  In fact, OSSUnixSubprocess can either work with regular files or with OSSPipe for dealing with stdin, stdout and stderr. 

OSSPipe uses a single-character buffer to implement #peek without losing data from the external OS pipe.