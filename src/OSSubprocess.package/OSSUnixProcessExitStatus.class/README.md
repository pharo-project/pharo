A OSSUnixProcessExitStatus represents the exit status of a unix process. This is an integer bit field answered by the wait() system call that contains information about exit status of the process. The meaning of the bit field varies according to the cause of process exit. 

When the OS process of a OSSUnixSubprocess exits and we collect the exit status via (#queryExitStatus which ends up doing the waitpid()), we get this bit integer bit fields. 
OSSUnixSubprocess #exitStatus answers this integer. To interpret it's result better, then we use this class OSSUnixProcessExitStatus (via #exitStatusInterpreter).
 
Ideally, we should take the resulting integer and call the macros WIFSIGNALED, WIFEXITED etc.. but since they are macros, they are not accessible via FFI. Therefore, we do the internal bit shits ourselves.

However, OSSUnixProcessExitStatus decodes the process exit status in a manner compatible with a typical GNU unix implementation. It is not guaranteed to be portable and may produce misleading results on other unix systems.

Following a normal process exit, the status may be decoded to provide a small positive integer value in the range 0 - 255, which is the value that is presented by a unix shell as the exit status of a program. If terminated by a signal, the corresponding value is the signal number of the signal that caused process exit.

