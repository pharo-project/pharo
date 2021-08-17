"
I am a an object representing a Unix platform. Use myself to access platform specific features.

Please keep me polymorphic to the the other platform objects.

To get the current platform, you can evaluate:

OSPlatform current.
"
Class {
	#name : #UnixPlatform,
	#superclass : #OSPlatform,
	#category : #'System-Platforms-Unix'
}

{ #category : #testing }
UnixPlatform class >> isActivePlatform [
	^ false
]

{ #category : #visiting }
UnixPlatform >> accept: aVisitor [

	^ aVisitor visitUnix: self
]

{ #category : #accessing }
UnixPlatform >> defaultMaximumPathLength [
	"This method returns the default maximum path length for the getCurrentWorkingDirectory implementation. 
Unix default PATH_MAX is 4096.  PATH_MAX is needed to avoid buffer overflow. In case PATH_MAX is not sufficient the method  currentWorkingDirectoryWithBufferSize: can be used to give desired buffer size."
	^4096

]

{ #category : #accessing }
UnixPlatform >> family [
	^#Unix
]

{ #category : #accessing }
UnixPlatform >> getPwdViaFFI: buffer size: bufferSize [
	"This method calls the Standard C Library getcwd() function. The name of the argument (arg1) should fit decompiled version.This method is used in getting the current working directory. getcwd is preferred over pwd because getcwd takes care of re-initialization of environment variables, whereas pwd needs implicit re- initialization.
	
	This method should be removed, as we should delegate to the VM
	
	We use FFI-OldFFIBackend for this because this is required for bootstrapping, and we ensure minimal dependencies."
	^ (ExternalLibraryFunction 
			name: 'getcwd'
			module: 'libc.so.6'
			callType: 1
			returnType: ExternalType char asPointerType
			argumentTypes: {
				ExternalType char asPointerType.
				ExternalType long })
					invokeWith: buffer with: bufferSize.

]

{ #category : #testing }
UnixPlatform >> isLinux [
	"Some Linux versions return linux, some linux-gnu, ..."
	
	^self version beginsWith: 'linux'
]

{ #category : #'testing - linux' }
UnixPlatform >> isLinuxDerivateOf: linuxOperatingSystemID [
	"Check if the installed OS is a derivate of the given linuxOperatingSystemID.
	 It returns true if the /etc/os-release file includes an entry ID_LIKE where the given
	 ID appears in the space separated list."
	
	| derivateList |
	derivateList := self linuxIdentificationParameters at: #ID_LIKE ifAbsent: [ ^false ]. 
	^(derivateList substrings: ' ') 
			anySatisfy: [:each | each asLowercase = linuxOperatingSystemID asLowercase ]
]

{ #category : #'testing - linux' }
UnixPlatform >> isLinuxDistributionWithID: linuxOperatingSystemID [
	"Check if the installed OS is a Linux with the given lowercase ID. One can
	 check for 'ubuntu', 'fedora', 'opensuse', 'rhel', 'centos' and others. 
	
	 A good list is available at: https://gist.github.com/natefoo/814c5bf936922dad97ff
	"
	
	| id |
	id := self linuxIdentificationParameters at: #ID ifAbsent: [ ^false ]. 
	^ id asLowercase = linuxOperatingSystemID asLowercase 
	
]

{ #category : #testing }
UnixPlatform >> isUnix [
	^ true
]

{ #category : #accessing }
UnixPlatform >> keyForValue: aKeyValue [
	^KeyboardKey valueForUnixPlatform: aKeyValue.
]

{ #category : #accessing }
UnixPlatform >> lineEnding [

	^ String lf
]

{ #category : #'accessing - linux' }
UnixPlatform >> linuxIdentificationParameters [
	"see https://www.freedesktop.org/software/systemd/man/os-release.html"

	| props extractor |
	props := Dictionary new.
	self isLinux ifFalse: [ ^props ].
	extractor := [ '/etc/os-release' asFileReference contents linesDo: [ :each | 
		props at: (each copyUpTo: $=) asSymbol put: (each copyAfter: $=) ]].

	"Do nothing when it does not exist"
	[ extractor value ] on: FileDoesNotExistException do: [ self error: 'Only available on distributions supporting the linux standard base (LSB)' ].
	^ props
]

{ #category : #accessing }
UnixPlatform >> menuShortcutString [
	^ 'ctrl'
]
