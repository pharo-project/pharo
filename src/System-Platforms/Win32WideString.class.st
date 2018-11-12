"
I represent a Win32 wide string, supporting non-ascii characters.
I manage the conversion between Pharo strings and Windows strings.

(Win32String fromString: 'âùö') asString = 'âùö'
"
Class {
	#name : #Win32WideString,
	#superclass : #FFIExternalObject,
	#category : #'System-Platforms-Windows'
}

{ #category : #'instance creation' }
Win32WideString class >> fromByteArray: byteArray [
	^ self new
		handle: byteArray;
		yourself
]

{ #category : #'instance creation' }
Win32WideString class >> fromHandle: handle [
	^ self new
		handle: handle;
		yourself
]

{ #category : #'instance creation' }
Win32WideString class >> fromString: aString [
	| r wideString anUTF8String codepage |
	wideString := self new: aString size.
	anUTF8String := aString utf8Encoded asString.
	codepage := 65001.

	r := OSPlatform current
		multiByteToWideCharacterCodepage: codepage
		flags: 0
		input: anUTF8String
		inputLen: anUTF8String size + 1
		output: wideString
		outputLen: wideString byteSize.

	r = 0 ifTrue: [ self error: 'Error while transforming utf8 string ', aString, ' using codepage ', codepage asString  ].
	
	^ wideString
]

{ #category : #'instance creation' }
Win32WideString class >> new: size [
	^ self new
		handle: (ByteArray new: (size + 1) * 2);
		yourself.

]

{ #category : #converting }
Win32WideString >> asString [
	| out r codepage |

	codepage := 65001.
	out := ByteArray new: (self size * 4) + 1.

	r := OSPlatform current
		wideCharacterToMultiByteCodepage: codepage
		flags: 0
		input: self
		inputLen: self size + 1
		output: out
		outputLen: out size.
		
	r = 0 ifTrue: [ self error: 'Error while transforming windows wide string using codepage ', codepage asString ].

	^ (out first: r - 1) utf8Decoded
]

{ #category : #converting }
Win32WideString >> asWin32WideString [
	^ self.
]

{ #category : #accessing }
Win32WideString >> byteSize [
	^ self handle isExternalAddress
		ifTrue: [ (self size + 1) * 2 ]
		ifFalse: [ self handle size ]
]

{ #category : #printing }
Win32WideString >> printOn: aStream [
	aStream 
		nextPutAll: 'a ' ;
		nextPutAll: self class name;
		nextPut: $(;
		print: self asString;
		nextPut: $)
]

{ #category : #accessing }
Win32WideString >> size [
	| size pos |
	size := 0.
	pos := 1.

	[ (self handle unsignedByteAt: pos) = 0 and: [ (self handle unsignedByteAt: pos + 1) = 0 ] ]
		whileFalse: [ size := size + 1.
			pos := pos + 2 ].

	^ size
]
