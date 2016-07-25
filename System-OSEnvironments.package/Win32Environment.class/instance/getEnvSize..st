private
getEnvSize: nameString
	^ self ffiCall: #( int GetEnvironmentVariableA ( String nameString, nil, 0 ) ) module: #Kernel32