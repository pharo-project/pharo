Extension { #name : 'WinPlatform' }

{ #category : '*System-OSEnvironments' }
WinPlatform >> environmentStrings [
	 ^ self ffiCall: #( void * GetEnvironmentStringsW () )
]

{ #category : '*System-OSEnvironments' }
WinPlatform >> environmentVariableNamed: aKey put: aValue [
	"The primitive on Windows currently uses the ascii version of the Windows API.
	In such chase try to set the value of the environment variable using FFI."

	| w32Key w32Value return |
	w32Key := aKey asWin32WideString.
	w32Value := aValue asWin32WideString.
	return := self setEnvironmentVariable: w32Key value: w32Value.

	"From MSDN: If the function fails, the return value is zero."
	return = 0 ifTrue: [ self error: 'An error occurred while setting environment variable ' , aKey asString , ' to ' , aValue asString ]
]

{ #category : '*System-OSEnvironments' }
WinPlatform >> environmentVariablesDo: aBlock [
	"Under windows the environemtn variables are a single big String."

	"Lines starting with an equal sign are invalid per
	  http://stackoverflow.com/questions/10431689/what-are-these-strange-environment-variables"

	| environmentStrings nextString win32WideString |
	environmentStrings := self environmentStrings.
	[
	win32WideString := Win32WideString fromHandle: environmentStrings.
	nextString := win32WideString asString.
	nextString ifEmpty: [ ^ self ].
	nextString first = $= ifFalse: [ self keysAndValuesDo: aBlock withAssociationString: nextString ].
	environmentStrings := environmentStrings + win32WideString byteSize ] repeat
]

{ #category : '*System-OSEnvironments' }
WinPlatform >> removeEnvironmentVariable: nameString [

	 ^ self ffiCall: #( int SetEnvironmentVariableW ( Win32WideString nameString, Win32WideString 0 ) )
]

{ #category : '*System-OSEnvironments' }
WinPlatform >> removeEnvironmentVariableNamed: aKey [
	"The primitive on Windows currently uses the ascii version of the Windows API.
	In such chase try to get value of the environment variable using FFI."

	| return |
	return := self removeEnvironmentVariable: aKey asWin32WideString.

	"From MSDN: If the function fails, the return value is zero."
	return = 0 ifTrue: [
		self error: 'An error occurred while removing environment variable ', aKey asString ]
]

{ #category : '*System-OSEnvironments' }
WinPlatform >> setEnv: nameString value: valueString [
	"This method calls the the platform specific set environment routine"

	^ self ffiCall: #( int SetEnvironmentVariableA #( String nameString #, String valueString ) ) module: #Kernel32
]

{ #category : '*System-OSEnvironments' }
WinPlatform >> setEnvironmentVariable: nameString value: valueString [

	 ^ self ffiCall: #( int SetEnvironmentVariableW ( Win32WideString nameString, Win32WideString valueString ) )
]
