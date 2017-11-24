"
I am a specialized OSEnvironment for Windows
"
Class {
	#name : #Win32Environment,
	#superclass : #OSEnvironment,
	#category : #'System-OSEnvironments'
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
	
	<todo>
	"The primitive on Windows is currently broken (2017-08-05) and instead of failing it can return nil.
	In such chase try to get value of the environment variable using FFI."

    | result | 
    
    result := super getEnv: aVariableName.
    ^ result ifNil: [self getEnvViaFFI: aVariableName ]
]

{ #category : #private }
Win32Environment >> getEnv: arg1 buffer: arg2 size: arg3 [
	"If the function succeeds, the return value is the number of characters stored in the buffer pointed to by aBuffer, not including the terminating null character.
	
If aBuffer is not large enough to hold the data, the return value is the buffer size, in characters, required to hold the string and its terminating null character and the contents of aBuffer are undefined.

If the function fails, the return value is zero. If the specified environment variable was not found in the environment block, GetLastError returns ERROR_ENVVAR_NOT_FOUND.

Important note: arguments of this method are named like the decompiler would use it so it could be used
                in the startup process for the case no source file is found.

    arg1 : a name as string representing the environment variable
    arg2 : the buffer
    arg3 : an integer with the size of the buffer
"
	 ^ self ffiCall: #( int GetEnvironmentVariableA ( String arg1, char *arg2, int arg3 ) ) module: #Kernel32
]

{ #category : #private }
Win32Environment >> getEnvSize: arg1 [
	"
	Return the buffer size of the given environment variable.

   Important note: arguments of this method are named like the decompiler would use 
   it so it could be used in the startup process for the case no source file is    
   found.

    arg1 : a name as string representing the environment variable	
	
	"
	^ self ffiCall: #( int GetEnvironmentVariableA ( String arg1, nil, 0 ) ) module: #Kernel32
]

{ #category : #private }
Win32Environment >> getEnvViaFFI: aVariableName [
	| valueSize buffer |
	valueSize := self getEnvSize: aVariableName.
	valueSize = 0 
		ifTrue: [ ^ nil ].
	buffer := ByteArray new: valueSize.
	(self getEnv: aVariableName buffer: buffer size: valueSize) = (valueSize - 1)
		ifFalse: [ ^ nil ].
	^ buffer allButLast asString
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
