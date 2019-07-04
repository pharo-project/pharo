"
I am a an object representing a Mac OS (pre OSX) platform. Use myself to access platform specific features.

"
Class {
	#name : #MacOSPlatform,
	#superclass : #OSPlatform,
	#category : #'System-Platforms-Mac'
}

{ #category : #testing }
MacOSPlatform class >> isActivePlatform [
	^ false
]

{ #category : #testing }
MacOSPlatform class >> isMacOS [

	^ self currentPlatformName = 'Mac OS'
]

{ #category : #visiting }
MacOSPlatform >> accept: aVisitor [

	^ aVisitor visitMacOS: self
]

{ #category : #accessing }
MacOSPlatform >> defaultMaximumPathLength [
		"This method returns the default maximum path length for the getCurrentWorkingDirectory implementation. MacOS default PATH_MAX is 1024.  PATH_MAX is needed to avoid buffer overflow. In case PATH_MAX is not sufficient the method currentWorkingDirectoryWithBufferSize: can be used to give desired buffer size."
	 ^1024

]

{ #category : #accessing }
MacOSPlatform >> family [
	^#MacOS
]

{ #category : #accessing }
MacOSPlatform >> getPwdViaFFI: buffer size: bufferSize [
	"This method calls the Standard C Library getcwd() function. The name of the argument (arg1) should fit decompiled version. This method is used in getting the current working directory. This uses the inbuilt libC function getcwd(). getcwd is preferred over pwd because getcwd takes care of re-initialization of environment variables, whereas pwd needs implict re- initialization.
	
	We use FFI-Kernel for this because this is required for bootstrapping, and we ensure minimal dependencies."
	^ (ExternalLibraryFunction 
			name: 'getcwd'
			module: 'libc.dylib'
			callType: 1
			returnType: ExternalType char asPointerType
			argumentTypes: {
				ExternalType char asPointerType.
				ExternalType long })
					invokeWith: buffer with: bufferSize.

]

{ #category : #testing }
MacOSPlatform >> isMacOS [
	^ true
]

{ #category : #accessing }
MacOSPlatform >> keyForValue: aKeyValue [
	^KeyboardKey valueForMacOSXPlatform: aKeyValue.
]

{ #category : #accessing }
MacOSPlatform >> lineEnding [

	^ String lf
]

{ #category : #accessing }
MacOSPlatform >> menuShortcutString [
	^ 'cmd'
]
