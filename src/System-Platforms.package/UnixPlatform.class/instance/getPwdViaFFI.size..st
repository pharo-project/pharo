*System-Platforms
getPwdViaFFI: arg1 size: arg2
	"This method calls the Standard C Library getcwd() function. The name of the argument (arg1) should fit decompiled version.This method is used in getting the current working directory. getcwd is preffered over pwd because getcwd takes care of re-initialization of environment variables, whereas pwd needs implict re- initialization."
	 ^ self ffiCall: #( String getcwd (String arg1,size_t arg2) ) module: LibC