"
A set of characters: 

- Lookups for inclusion are very fast.
- CharacterSet will automatically convert itself to a WideCharacterSet if a character with value > 255 is added.
- Tests for inclusion can be done on any Character value (up to Character maxVal).

See the package comments for a description of how each of the classes in Collections-Support-CharacterSets is used.
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
	#category : #'Collections-Support-CharacterSets'
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
CharacterSet class >> newFrom: aCollection [
	| newCollection |
	newCollection := self new.
	newCollection addAll: aCollection.
	^newCollection
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

{ #category : #comparing }
CharacterSet >> = anObject [
	^self species == anObject species and: [
		self byteArrayMap = anObject byteArrayMap ]
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

{ #category : #converting }
CharacterSet >> byteComplement [
	"return a character set containing precisely the single byte characters the receiver does not"
	
	| set |
	set := self class allCharacters.
	self do: [ :c | set remove: c ].
	^set
]

{ #category : #converting }
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
CharacterSet >> includes: aCharacter [
	"Guard against wide characters"
	^aCharacter asciiValue >= 256
		ifTrue: [ false ]
		ifFalse: [ (map at: aCharacter asciiValue + 1) > 0 ]
]

{ #category : #initialization }
CharacterSet >> initialize [
	super initialize.
	map := ByteArray new: 256 withAll: 0.
]

{ #category : #copying }
CharacterSet >> postCopy [
	super postCopy.
	map := map copy
]

{ #category : #removing }
CharacterSet >> remove: aCharacter [
	aCharacter asciiValue >= 256
		ifFalse: ["Guard against wide characters"
			map at: aCharacter asciiValue + 1 put: 0].
	^aCharacter
]

{ #category : #removing }
CharacterSet >> remove: aCharacter ifAbsent: aBlock [
	(self includes: aCharacter) ifFalse: [^aBlock value].
	^self remove: aCharacter
]

{ #category : #removing }
CharacterSet >> removeAll [

	map atAllPut: 0
]

{ #category : #accessing }
CharacterSet >> size [
	^map sum
]

{ #category : #private }
CharacterSet >> wideCharacterMap [
	"used for comparing with WideCharacterSet"
	
	| wide |
	wide := WideCharacterSet new.
	wide addAll: self.
	^wide wideCharacterMap
]
