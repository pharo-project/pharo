private
getEnv: nameString buffer: aBuffer size: anInteger
	"If the function succeeds, the return value is the number of characters stored in the buffer pointed to by aBuffer, not including the terminating null character.
	
If aBuffer is not large enough to hold the data, the return value is the buffer size, in characters, required to hold the string and its terminating null character and the contents of aBuffer are undefined.

If the function fails, the return value is zero. If the specified environment variable was not found in the environment block, GetLastError returns ERROR_ENVVAR_NOT_FOUND."
	 ^ self ffiCall: #( int GetEnvironmentVariableA ( String nameString, char *aBuffer, int anInteger ) ) module: #Kernel32