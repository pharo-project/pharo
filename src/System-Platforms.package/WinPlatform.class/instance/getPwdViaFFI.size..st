accessing
getPwdViaFFI: buffer size: bufferSize
	"This method calls the Standard C Library getcwd() function. The name of the argument (arg1) should fit decompiled version. This method is used in getting the current working directory. getcwd is preffered over pwd because getcwd takes care of re-initialization of environment variables, whereas pwd needs implict re- initialization.
	
	We use FFI-Kernel for this because this is required for bootstrapping, and we ensure minimal dependencies."
	^ (ExternalLibraryFunction 
			name: '_getcwd'
			module: 'msvcrt.dll'
			callType: 0
			returnType: ExternalType char asPointerType
			argumentTypes: {
				ExternalType char asPointerType.
				ExternalType long })
					invokeWith: buffer with: bufferSize.
