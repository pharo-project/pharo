"
A set of characters.  Lookups for inclusion are very fast.

map is a ByteArray of size 256, initially all zeros.  
map at: n+1 is set to 1 when a Character with a codePoint of n is added to the set.

If a ""wide"" character (codePoint > 255) is added, the ChacracterSet becomes a WideCharacterSet.
 
"
Class {
	#name : #CharacterSet,
	#superclass : #Collection,
	#instVars : [
		'map'
	],
	#classVars : [
		'CrLf'
	],
	#category : #'Collections-Support'
}

{ #category : #'instance creation' }
CharacterSet class >> allCharacters [
	"return a set containing all characters"

	| set |
	set := self empty.
	0 to: 255 do: [ :ascii | set add: (Character value: ascii) ].
	^set
]

{ #category : #accessing }
CharacterSet class >> crlf [
	CrLf ifNil: [CrLf := self with: Character cr with: Character lf].
	^CrLf
]

{ #category : #'instance creation' }
CharacterSet class >> empty [
 	"return an empty set of characters"
	^self new
]

{ #category : #'instance creation' }
CharacterSet class >> newFrom: aCollection [
	| newCollection |
	newCollection := self new.
	newCollection addAll: aCollection.
	^ newCollection
]

{ #category : #'instance creation' }
CharacterSet class >> nonSeparators [
	"return a set containing everything but the whitespace characters"

	^self separators complement
]

{ #category : #'instance creation' }
CharacterSet class >> separators [
	"return a set containing just the whitespace characters"

	| set |
	set := self empty.
	set addAll: Character separators.
	^set
]

{ #category : #'instance creation' }
CharacterSet class >> withAll: aCollection [
	^ self newFrom: aCollection
]

{ #category : #'instance creation' }
CharacterSet class >> without: aSetOfCharacters [
	"return a set containing the inverse of aSetOfCharacters"

	^ CharacterSetComplement of: aSetOfCharacters
]

{ #category : #comparing }
CharacterSet >> = anObject [
	self class == anObject class
		ifTrue: [ ^ self byteArrayMap = anObject byteArrayMap ].
	((anObject respondsTo: #size) and: [ anObject respondsTo: #includes: ])
		ifFalse: [ ^ false ].
	self size = anObject size
		ifFalse: [ ^ false ].
	self do: [ :each | 
			(anObject includes: each) ifFalse: [ ^ false ] ].
	^ true
]

{ #category : #'collection ops' }
CharacterSet >> abstractSize [
	"an abstraction of my size, to be used in implementing union & intersection"
		
	^ Size small
]

{ #category : #adding }
CharacterSet >> add: aCharacter [
	"I automatically become a WideCharacterSet if you add a wide character to myself"
	
	aCharacter asciiValue >= 256
		ifTrue: [| wide |
			wide := WideCharacterSet new.
			wide addAll: self.
			wide add: aCharacter.
			self becomeForward: wide.
			^aCharacter].
	map at: aCharacter asciiValue + 1 put: 1.
	^aCharacter
]

{ #category : #converting }
CharacterSet >> asString [
	"Convert the receiver into a String"

	^String 
		new: self size 
		streamContents: 
			[ :s | self do: [ :ch | s nextPut: ch]].
]

{ #category : #private }
CharacterSet >> byteArrayMap [
	"return a ByteArray mapping each ascii value to a 1 if that ascii value is in the set, and a 0 if it isn't.  Intended for use by primitives only"
	^map
]

{ #category : #conversion }
CharacterSet >> byteComplement [
	"return a character set containing precisely the single byte characters the receiver does not"
	
	| set |
	set := CharacterSet allCharacters.
	self do: [ :c | set remove: c ].
	^set
]

{ #category : #conversion }
CharacterSet >> complement [
	"return a character set containing precisely the characters the receiver does not"
	
	^CharacterSetComplement of: self copy
]

{ #category : #enumerating }
CharacterSet >> do: aBlock [
	"evaluate aBlock with each character in the set"

	Character allByteCharacters do: [ :c |
		(self includes: c) ifTrue: [ aBlock value: c ] ]

]

{ #category : #enumerating }
CharacterSet >> findFirstInByteString: aByteString startingAt: startIndex [
	"Double dispatching: since we know this is a ByteString, we can use a superfast primitive using a ByteArray map with 0 slots for byte characters not included and 1 for byte characters included in the receiver."
	^ByteString
		findFirstInString: aByteString
		inSet: self byteArrayMap
		startingAt: startIndex
]

{ #category : #testing }
CharacterSet >> hasWideCharacters [
	^false
]

{ #category : #comparing }
CharacterSet >> hash [
	^self byteArrayMap hash
]

{ #category : #testing }
CharacterSet >> identityIncludes: aCharacter [
	"On Characters, = and == are the same"
	^ self includes: aCharacter
]

{ #category : #'collection ops' }
CharacterSet >> includes: aCharacter [
	aCharacter asciiValue >= 256
		ifTrue: ["Guard against wide characters"
			^false].
	^(map at: aCharacter asciiValue + 1) > 0
]

{ #category : #initialization }
CharacterSet >> initialize [
	super initialize.
	map := ByteArray new: 256 withAll: 0.
]

{ #category : #private }
CharacterSet >> intersectSmallWithLarge: aLargerSet [
	^ self select: [ :each | (aLargerSet includes: each) ]
]

{ #category : #enumerating }
CharacterSet >> intersection: anotherSet [
	| smaller larger |
	(anotherSet abstractSize = Size small and: [ self size < anotherSet size ])
		ifTrue: [ smaller := self.
			larger := anotherSet ]
		ifFalse: [ smaller := anotherSet.
			larger := self ].
	^ smaller intersectSmallWithLarge: larger
]

{ #category : #accessing }
CharacterSet >> like: aCharacter [
	"Answer aCharacter if aCharacter is in the receiver, or nil if it is absent."

	^ (self includes: aCharacter) ifTrue: [ aCharacter ] ifFalse: [ nil ]
]

{ #category : #copying }
CharacterSet >> postCopy [
	super postCopy.
	map := map copy
]

{ #category : #removing }
CharacterSet >> remove: aCharacter [
	^ self remove: aCharacter ifAbsent: [ self errorNotFound: aCharacter ]
]

{ #category : #removing }
CharacterSet >> remove: aCharacter ifAbsent: aBlock [
	| code |
	code := aCharacter asciiValue.
	(code >= 256 or: [ (map at: code + 1) == 0 ]) ifTrue: [ ^ aBlock value ].
	map at: code + 1 put: 0.
	^ aCharacter
]

{ #category : #removing }
CharacterSet >> removeAll [

	map atAllPut: 0
]

{ #category : #'collection ops' }
CharacterSet >> size [
	^map sum
]

{ #category : #enumerating }
CharacterSet >> union: anotherSet [
	| smaller larger |
	(anotherSet abstractSize = Size small and: [ self size < anotherSet size ])
		ifTrue: [ smaller := self.
			larger := anotherSet ]
		ifFalse: [ smaller := anotherSet.
			larger := self ].
	^ larger unionLargeWithSmall: smaller
]

{ #category : #enumerating }
CharacterSet >> unionLargeWithSmall: aSmallerSet [
	^ self copy addAll: aSmallerSet; yourself
]

{ #category : #private }
CharacterSet >> wideCharacterMap [
	"used for comparing with WideCharacterSet"
	
	| wide |
	wide := WideCharacterSet new.
	wide addAll: self.
	^wide wideCharacterMap
]
