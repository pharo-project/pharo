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
	| name buffer return |

	name := aVariableName asWin32WideString.
	buffer := Win32WideString new: 500.
	return := OSPlatform current getEnvironmentVariable: name into: buffer size: 500.
	
	return = 0
		ifTrue: [ self error: 'Error while getting environment variable ', aVariableName ].
	return > 500
		ifTrue: [ self error: 'Not enough buffer space' ].
	
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
