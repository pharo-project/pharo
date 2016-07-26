private
setEnv: nameString value: valueString
	"This method calls the Standard C Library getenv() function"
	^ self ffiCall: #( int setenv (String nameString, String valueString, 1) ) module: LibC