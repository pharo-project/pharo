"
I am a specialized OSEnvironment for Windows
"
Class {
	#name : #Win32Environment,
	#superclass : #OSEnvironment,
	#category : #'System-OSEnvironments-Platforms'
}

{ #category : #testing }
Win32Environment class >> isDefaultFor: aPlatform [ 
	^ aPlatform isWin32
]

{ #category : #accessing }
Win32Environment >> at: aKey ifAbsent: aBlock [
	"Gets the value of an environment variable called `aKey`.
	Execute aBlock if absent.
	It is the system reponsibility to manage the encoding.
	
	Rationale: A common denominator for all platforms providing an already decoded string, because windows does not (compared to *nix systems) provide a encoded byte representation of the value. Windows has instead its own wide string representation."
	
	"The primitive on Windows currently uses the ascii version of the Windows API.
	In such chase try to get value of the environment variable using FFI."
	^ self getEnvViaFFI: aKey bufferSize: 500 ifAbsent: aBlock
]

{ #category : #accessing }
Win32Environment >> at: aKey put: aValue [

	| w32Key w32Value return |
	w32Key := aKey asWin32WideString.
	w32Value := aValue asWin32WideString.
	return := OSPlatform current setEnvironmentVariable: w32Key value: w32Value.	

	"From MSDN: If the function fails, the return value is zero."
	return = 0 ifTrue: [ 
		self error: 'An error occurred while setting environment variable ', aKey asString, ' to ', aValue asString ].
]

{ #category : #private }
Win32Environment >> environmentStrings [
	 ^ self ffiCall: #( void * GetEnvironmentStrings () ) module: #Kernel32
]

{ #category : #private }
Win32Environment >> getEnvViaFFI: aVariableName bufferSize: aSize ifAbsent: aBlock [
	| name buffer return |

	name := aVariableName asWin32WideString.
	buffer := Win32WideString new: aSize.
	return := OSPlatform current getEnvironmentVariable: name into: buffer size: aSize + 1.
	
	"From MSDN: If the function fails, the return value is zero. If the specified environment variable was not found in the environment block, GetLastError returns ERROR_ENVVAR_NOT_FOUND."
	return = 0 ifTrue: [ 
		OSPlatform current lastError = "ERROR_ENVVAR_NOT_FOUND" 16r000000CB
			ifTrue: [ ^ aBlock value ]
			ifFalse: [ self error: 'An error occurred while fetching environment variable ', aVariableName asString ] ].
	
	"From MSDN: If lpBuffer is not large enough to hold the data, the return value is the buffer size, in characters,
	required to hold the string and its terminating null character and the contents of lpBuffer are undefined."
	return > aSize ifTrue: [ ^ self getEnvViaFFI: aVariableName bufferSize: return ifAbsent: aBlock ].
	
	^ buffer asString
]

{ #category : #enumeration }
Win32Environment >> keysAndValuesDo: aBlock [
	"Under windows the environemtn variables are a single big String."

	"Lines starting with an equal sign are invalid per    
	  http://stackoverflow.com/questions/10431689/what-are-these-strange-environment-variables"
	| environmentStrings nextString |

	environmentStrings := self environmentStrings.
	[ 
	nextString := environmentStrings fromCString.
	nextString ifEmpty: [ ^ self ].
	nextString first = $=
		ifFalse: [ self keysAndValuesDo: aBlock withAssociationString: nextString ].
	environmentStrings := environmentStrings + nextString size + 1 ] repeat
]

{ #category : #accessing }
Win32Environment >> removeKey: aKey [
	
	| return |
	return := OSPlatform current removeEnvironmentVariable: aKey asWin32WideString.

	"From MSDN: If the function fails, the return value is zero."
	return = 0 ifTrue: [ 
		self error: 'An error occurred while removing environment variable ', aKey asString ].
]
