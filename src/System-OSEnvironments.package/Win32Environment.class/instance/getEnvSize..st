private
getEnvSize: arg1
	"
	Return the buffer size of the given environment variable.

   Important note: arguments of this method are named like the decompiler would use 
   it so it could be used in the startup process for the case no source file is    
   found.

    arg1 : a name as string representing the environment variable	
	
	"
	^ self ffiCall: #( int GetEnvironmentVariableA ( String arg1, nil, 0 ) ) module: #Kernel32