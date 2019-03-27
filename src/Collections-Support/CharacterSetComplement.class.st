"
CharacterSetComplement is a space efficient implementation of (CharacterSet complement) taking care of WideCharacter (code > 255)

However, it will maintain a byteArrayMap for character <= 255 in a cache for performance

instance variables:
	absent <CharacterSet> contains character that are not in the set (i.e. my complement)
	byteArrayMapCache <ByteArray | nil> cache this information because it has to be used in tight loops where efficiency matters
"
Class {
	#name : #CharacterSetComplement,
	#superclass : #Collection,
	#instVars : [
		'absent',
		'byteArrayMapCache'
	],
	#category : #'Collections-Support-CharacterSets'
}

{ #category : #'instance creation' }
CharacterSetComplement class >> of: aCharacterSet [
	"answer the complement of aCharacterSet"
	
	^ super new complement: aCharacterSet
]

{ #category : #comparing }
CharacterSetComplement >> = anObject [
	"Implementation note: we do not test if equal to a WideCharacterSet,
	because it is unlikely that WideCharacterSet is as complete as self"
	
	^self class == anObject class and: [
		absent = anObject complement ]
]

{ #category : #adding }
CharacterSetComplement >> add: aCharacter [ 
	"a character is present if not absent, so adding a character is removing it from the absent"
	
	(absent includes: aCharacter)
		ifTrue:
			[byteArrayMapCache := nil.
			absent remove: aCharacter].
	^ aCharacter
]

{ #category : #private }
CharacterSetComplement >> byteArrayMap [
	"return a ByteArray mapping each ascii value to a 1 if that ascii value is in the set, and a 0 if it isn't.  Intended for use by primitives only"

	^byteArrayMapCache ifNil: [byteArrayMapCache := absent byteArrayMap collect: [:i | 1 - i]]
]

{ #category : #converting }
CharacterSetComplement >> complement [
	"return a character set containing precisely the characters the receiver does not"
	
	^absent copy
]

{ #category : #initialization }
CharacterSetComplement >> complement: aCharacterSet [
	"initialize with the complement"
	
	byteArrayMapCache := nil.
	absent := aCharacterSet.
	
]

{ #category : #enumerating }
CharacterSetComplement >> do: aBlock [
	"evaluate aBlock with each character in the set"

	0 to: self size - 1 do: [ :codePoint | | ch |
		ch := Character value: codePoint.
		(self includes: ch) ifTrue: [ aBlock value: ch ] ]
]

{ #category : #enumerating }
CharacterSetComplement >> findFirstInByteString: aByteString startingAt: startIndex [
	"Double dispatching: since we know this is a ByteString, we can use a superfast primitive using a ByteArray map with 0 slots for byte characters not included and 1 for byte characters included in the receiver."
	^ByteString
		findFirstInString: aByteString
		inSet: self byteArrayMap
		startingAt: startIndex
]

{ #category : #testing }
CharacterSetComplement >> hasWideCharacters [
	"This is a guess that absent is not holding each and every possible wideCharacter..."
	
	^true
]

{ #category : #comparing }
CharacterSetComplement >> hash [
	^absent hash bitXor: self class hash
]

{ #category : #testing }
CharacterSetComplement >> includes: aCharacter [
	^(absent includes: aCharacter) not
]

{ #category : #copying }
CharacterSetComplement >> postCopy [
	super postCopy.
	absent := absent copy
]

{ #category : #printing }
CharacterSetComplement >> printOn: aStream [
	"Print a description of the complement rather than self.
	Rationale: self would be too long to print."
	
	aStream nextPut: $(.
	absent printOn: aStream.
	aStream nextPut: $); space; nextPutAll: #complement.
]

{ #category : #enumerating }
CharacterSetComplement >> reject: aBlock [
	"Implementation note: rejecting present is selecting absent"
	
	^(absent select: aBlock) complement
]

{ #category : #removing }
CharacterSetComplement >> remove: aCharacter [
	"This means aCharacter is now absent from myself.
	It must be added to my absent."
	
	byteArrayMapCache := nil.
	^absent add: aCharacter
]

{ #category : #removing }
CharacterSetComplement >> remove: aCharacter ifAbsent: aBlock [
	(self includes: aCharacter) ifFalse: [^aBlock value].
	^self remove: aCharacter
]

{ #category : #removing }
CharacterSetComplement >> removeAll [
	self becomeForward: CharacterSet new
]

{ #category : #enumerating }
CharacterSetComplement >> select: aBlock [
	"Implementation note: selecting present is rejecting absent"
	
	^(absent reject: aBlock) complement
]

{ #category : #accessing }
CharacterSetComplement >> size [
	"The size is all characters minus those explicitly excluded"

	"Character values include 0, so we need to add 1."
	^Character maxVal + 1 - absent size
]

{ #category : #printing }
CharacterSetComplement >> storeOn: aStream [
	"Store a description of the elements of the complement rather than self."
	
	aStream nextPut: $(.
	absent storeOn: aStream.
	aStream nextPut: $); space; nextPutAll: #complement.
]
