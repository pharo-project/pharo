Extension { #name : 'WinPlatform' }

{ #category : '*System-OSEnvironments' }
WinPlatform >> doGetEnvVariable: aVariableName bufferSize: aSize ifAbsent: aBlock isRetry: isRetry [

	| name buffer return lastErrCode |
	name := aVariableName asWin32WideString.
	buffer := Win32WideString new: aSize.

	"This Windows API to get the environment variable has ambiguous behavior when handling environment variables with empty values.
	In the case of a empty string, the API call to get the environment variable returns 0.
	We need to differentiate the 0 in the case of an empty string and when there is an error.
	To do so, we are clearing the last error variable, and also we are retrying.
	The retrying is needed because we can have a race condition between different API calls."

	lastErrCode := self setLastError: 0.
	return := self getEnvironmentVariable: name into: buffer size: aSize + 1.
	lastErrCode := self lastError.

	"From MSDN: If the function fails, the return value is zero. If the specified environment variable was not found in the environment block, GetLastError returns ERROR_ENVVAR_NOT_FOUND."
	return = 0 ifTrue: [
		lastErrCode = 0 ifTrue: [ ^ String new ].
		lastErrCode = 16r000000CB ifTrue: [ ^ aBlock value ]. "ERROR_ENVVAR_NOT_FOUND"

		isRetry
			ifTrue: [ self error: 'Error ' , lastErrCode printString , ' occurred while fetching environment variable ' , aVariableName asString ]
			ifFalse: [
				^ self
					  doGetEnvVariable: aVariableName
					  bufferSize: aSize
					  ifAbsent: aBlock
					  isRetry: true ] ].

	"From MSDN: If lpBuffer is not large enough to hold the data, the return value is the buffer size, in characters,
	required to hold the string and its terminating null character and the contents of lpBuffer are undefined."
	return > aSize ifTrue: [
		^ self
			  doGetEnvVariable: aVariableName
			  bufferSize: return
			  ifAbsent: aBlock
			  isRetry: false ].

	^ buffer asString
]

{ #category : '*System-OSEnvironments' }
WinPlatform >> environmentStrings [
	 ^ self ffiCall: #( void * GetEnvironmentStringsW () )
]

{ #category : '*System-OSEnvironments' }
WinPlatform >> environmentVariableNamed: aKey ifAbsent: aBlock [
	"The primitive on Windows currently uses the ascii version of the Windows API.
	In such chase try to get value of the environment variable using FFI."

	^ self getEnvVariable: aKey bufferSize: 500 ifAbsent: aBlock
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
WinPlatform >> getEnvVariable: aVariableName bufferSize: aSize ifAbsent: aBlock [

	^ self doGetEnvVariable: aVariableName bufferSize: aSize ifAbsent: aBlock isRetry: false
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
