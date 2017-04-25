private
getEnv: arg1 buffer: arg2 size: arg3
	"If the function succeeds, the return value is the number of characters stored in the buffer pointed to by aBuffer, not including the terminating null character.
	
If aBuffer is not large enough to hold the data, the return value is the buffer size, in characters, required to hold the string and its terminating null character and the contents of aBuffer are undefined.

If the function fails, the return value is zero. If the specified environment variable was not found in the environment block, GetLastError returns ERROR_ENVVAR_NOT_FOUND.

Important note: arguments of this method are named like the decompiler would use it so it could be used
                in the startup process for the case no source file is found.

    arg1 : a name as string representing the environment variable
    arg2 : the buffer
    arg3 : an integer with the size of the buffer
"
	 ^ self ffiCall: #( int GetEnvironmentVariableA ( String arg1, char *arg2, int arg3 ) ) module: #Kernel32