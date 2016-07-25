private
getEnv: arg1
	"This method calls the Standard C Library getenv() function. The name of the argument (arg1) should fit decompiled version."
	 ^ self ffiCall: #( String getenv (String arg1) ) module: LibC