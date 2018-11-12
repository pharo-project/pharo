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
Win32Environment >> environmentStrings [
	 ^ self ffiCall: #( void * GetEnvironmentStrings () ) module: #Kernel32
]

{ #category : #accessing }
Win32Environment >> getEnv: aVariableName [
	"The primitive on Windows currently uses the ascii version of the Windows API.
	In such chase try to get value of the environment variable using FFI."
    ^ self getEnvViaFFI: aVariableName
]

{ #category : #private }
Win32Environment >> getEnvViaFFI: aVariableName [

	^ self getEnvViaFFI: aVariableName bufferSize: 500
]

{ #category : #private }
Win32Environment >> getEnvViaFFI: aVariableName bufferSize: aSize [
	| name buffer return |

	name := aVariableName asWin32WideString.
	buffer := Win32WideString new: aSize.
	return := OSPlatform current getEnvironmentVariable: name into: buffer size: aSize + 1.
	
	"From MSDN: If the function fails, the return value is zero. If the specified environment variable was not found in the environment block, GetLastError returns ERROR_ENVVAR_NOT_FOUND."
	return = 0 ifTrue: [ 
		OSPlatform current lastError = 16r000000CB
			ifTrue: [ ^ nil ]
			ifFalse: [ self error: 'An error occurred while fetching environment variable ', aVariableName asString ] ].
	
	"From MSDN: If lpBuffer is not large enough to hold the data, the return value is the buffer size, in characters,
	required to hold the string and its terminating null character and the contents of lpBuffer are undefined."
	return > aSize ifTrue: [ ^ self getEnvViaFFI: aVariableName bufferSize: return ].
	
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

{ #category : #private }
Win32Environment >> setEnv: nameString value: valueString [
	"This method calls the Standard C Library getenv() function"
	 ^ self ffiCall: #( int SetEnvironmentVariableA ( String nameString, String valueString ) ) module: #Kernel32
]

{ #category : #accessing }
Win32Environment >> unsetEnv: aString [
	^ self setEnv: aString value: nil
]
