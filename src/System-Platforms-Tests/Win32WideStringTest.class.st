Class {
	#name : #Win32WideStringTest,
	#superclass : #TestCase,
	#category : #'System-Platforms-Tests-Win32'
}

{ #category : #tests }
Win32WideStringTest >> exampleString [

	^ 'áéíóúñÑâîôûêèàìòù©¢ßðöäå®þü¼¡²³¤€¼½¾‘’¥'
]

{ #category : #testing }
Win32WideStringTest >> isWindows [

	^ OSPlatform current isWindows
]

{ #category : #tests }
Win32WideStringTest >> testCharactersAreEncodedInUnicode16Bits [

	| bytes index |

	self isWindows
		ifFalse: [ ^self skip ].

	bytes := self exampleString asWin32WideString getHandle.
	index := 1.

	self exampleString do: [ :aCharacter |
		self assert: (bytes at: index) equals: (aCharacter codePoint bitAnd: 255).
		self assert: (bytes at: index+1) equals: (aCharacter codePoint >> 8).
		index := index + 2.
	]
]

{ #category : #tests }
Win32WideStringTest >> testConvertingInBothDirectionsGaveSameString [

	self isWindows
		ifFalse: [ ^self skip ].

	self assert: self exampleString asWin32WideString asString equals: self exampleString
]

{ #category : #tests }
Win32WideStringTest >> testHandleIsAByteArray [

	self isWindows
		ifFalse: [ ^self skip ].

	self assert: (self exampleString asWin32WideString getHandle isKindOf: ByteArray)
]

{ #category : #tests }
Win32WideStringTest >> testUnderlayingByteArrayEndsInTwoZeros [

	self isWindows
		ifFalse: [ ^self skip ].

	self assert: (self exampleString asWin32WideString getHandle last:2) equals: #[00 00]
]

{ #category : #tests }
Win32WideStringTest >> testUnderlayingByteArrayIsMultipleOf2 [

	self isWindows
		ifFalse: [ ^self skip ].

	self assert: (self exampleString asWin32WideString getHandle size) % 2 equals: 0
]
