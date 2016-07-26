private
setEnv: nameString value: valueString
	"This method calls the Standard C Library getenv() function"
	 ^ self ffiCall: #( int SetEnvironmentVariableA ( String nameString, String valueString ) ) module: #Kernel32