private
unsetEnv: string
	"This method calls the Standard C Library getenv() function"
	 ^ self ffiCall: #( int unsetenv (String string) ) module: LibC