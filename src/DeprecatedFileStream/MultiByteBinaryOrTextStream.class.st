"
It is similar to MultiByteFileStream, but works on in memory stream.
"
Class {
	#name : #MultiByteBinaryOrTextStream,
	#superclass : #ReadWriteStream,
	#instVars : [
		'isBinary',
		'converter'
	],
	#category : #'DeprecatedFileStream-Base'
}

{ #category : #defaults }
MultiByteBinaryOrTextStream class >> defaultConverter [
	^ Latin1TextConverter new.

]

{ #category : #deprecation }
MultiByteBinaryOrTextStream class >> isDeprecated [
	^true
]

{ #category : #'instance creation' }
MultiByteBinaryOrTextStream class >> on: aCollection encoding: encodingName [ 
	| aTextConverter |
	encodingName 
		ifNil: [aTextConverter := TextConverter default]
		ifNotNil: [aTextConverter := TextConverter newForEncoding: encodingName].
	^ (self on: aCollection)
		converter: aTextConverter
]

{ #category : #'instance creation' }
MultiByteBinaryOrTextStream class >> with: aCollection encoding: encodingName [ 
	| aTextConverter |
	encodingName 
		ifNil: [aTextConverter := TextConverter default]
		ifNotNil: [aTextConverter := TextConverter newForEncoding: encodingName].
	^ (self with: aCollection)
		converter: aTextConverter
]

{ #category : #converting }
MultiByteBinaryOrTextStream >> asBinaryOrTextStream [

	^ self

]

{ #category : #modes }
MultiByteBinaryOrTextStream >> ascii [
	isBinary := false

]

{ #category : #private }
MultiByteBinaryOrTextStream >> basicNext [
	| nextChar |

	^ isBinary
		ifTrue: [super next]
		ifFalse: [
			(nextChar := super next)
				ifNotNil: [nextChar asCharacter]].
]

{ #category : #private }
MultiByteBinaryOrTextStream >> basicNext: anInteger [

	^ super next: anInteger.

]

{ #category : #private }
MultiByteBinaryOrTextStream >> basicNext: n into: aString [

	^ self next: n into: aString.

]

{ #category : #private }
MultiByteBinaryOrTextStream >> basicNext: anInteger putAll: aCollection startingAt: startIndex [
	
	^super next: anInteger putAll: aCollection startingAt: startIndex
]

{ #category : #private }
MultiByteBinaryOrTextStream >> basicNextInto: aString [

	^ self nextInto: aString.

]

{ #category : #private }
MultiByteBinaryOrTextStream >> basicNextPut: char [

	^ super nextPut: char.

]

{ #category : #private }
MultiByteBinaryOrTextStream >> basicNextPutAll: aString [

	^ super nextPutAll: aString.

]

{ #category : #private }
MultiByteBinaryOrTextStream >> basicPeek [

	^ super peek

]

{ #category : #private }
MultiByteBinaryOrTextStream >> basicPosition [

	^ self position.

]

{ #category : #private }
MultiByteBinaryOrTextStream >> basicPosition: pos [

	^ self position: pos.

]

{ #category : #modes }
MultiByteBinaryOrTextStream >> binary [
	isBinary := true

]

{ #category : #accessing }
MultiByteBinaryOrTextStream >> contents [

	| ret state |
	state := converter saveStateOf: self.
	ret := self upToEnd.
	converter restoreStateOf: self with: state.
	^ ret.

]

{ #category : #accessing }
MultiByteBinaryOrTextStream >> converter [

	converter ifNil: [converter := self class defaultConverter].
	^ converter

]

{ #category : #accessing }
MultiByteBinaryOrTextStream >> converter: aConverter [

	converter := aConverter.

]

{ #category : #testing }
MultiByteBinaryOrTextStream >> isBinary [
	^ isBinary
]

{ #category : #accessing }
MultiByteBinaryOrTextStream >> next [

	| n |
	n := self converter nextFromStream: self.
	n ifNil: [^ nil].
	isBinary and: [n isCharacter ifTrue: [^ n asciiValue]].
	^ n.

]

{ #category : #accessing }
MultiByteBinaryOrTextStream >> next: anInteger [ 

	| multiString |
	self isBinary ifTrue: [^ (super next: anInteger) asByteArray].
	multiString := WideString new: anInteger.
	1 to: anInteger do: [:index |
		| character |
		(character := self next) ifNotNil: [
			multiString at: index put: character
		] ifNil: [
			multiString := multiString copyFrom: 1 to: index - 1.
			^ multiString
		]
	].
	^ multiString.

]

{ #category : #accessing }
MultiByteBinaryOrTextStream >> next: anInteger putAll: aCollection startingAt: startIndex [


        (self isBinary or: [ aCollection class == ByteArray ]) ifTrue: [
                ^super next: anInteger putAll: aCollection startingAt: startIndex ].
        ^self converter next: anInteger putAll: aCollection startingAt: startIndex toStream: self
]

{ #category : #accessing }
MultiByteBinaryOrTextStream >> nextDelimited: terminator [

	| out ch pos |
	out := (String new: 1000) writeStream.
	self atEnd ifTrue: [^ ''].
	pos := self position.
	self next = terminator ifFalse: [
		"absorb initial terminator"
		self position: pos.
	].
	[(ch := self next) == nil] whileFalse: [
		(ch = terminator) ifTrue: [
			self peek = terminator ifTrue: [
				self next.  "skip doubled terminator"
			] ifFalse: [
				^ out contents  "terminator is not doubled; we're done!"
			].
		].
		out nextPut: ch.
	].
	^ out contents.

]

{ #category : #accessing }
MultiByteBinaryOrTextStream >> nextMatchAll: aColl [

    | save |
    save := converter saveStateOf: self.
    aColl do: [:each |
       (self next) = each ifFalse: [
            converter restoreStateOf: self with: save.
            ^ false.
		].
	].
    ^ true.

]

{ #category : #accessing }
MultiByteBinaryOrTextStream >> nextPut: aCharacter [
	^ aCharacter isInteger 
		ifTrue: [ super nextPut: aCharacter asCharacter ]
		ifFalse: [ self converter nextPut: aCharacter toStream: self ]
]

{ #category : #accessing }
MultiByteBinaryOrTextStream >> nextPutAll: aCollection [
	^ self isBinary
		ifTrue: [ super nextPutAll: aCollection ]
		ifFalse: [ aCollection do: [ :each | self nextPut: each ] ]
]

{ #category : #accessing }
MultiByteBinaryOrTextStream >> padToEndWith: aChar [
	"We don't have pages, so we are at the end, and don't need to pad."
]

{ #category : #accessing }
MultiByteBinaryOrTextStream >> peek [
	"Answer what would be returned if the message next were sent to the receiver. If the receiver is at the end, answer nil.  "

	| next pos |
	self atEnd ifTrue: [^ nil].
	pos := self position.
	next := self next.
	self position: pos.
	^ next.


]

{ #category : #accessing }
MultiByteBinaryOrTextStream >> peekFor: item [ 

	| next state |
	"self atEnd ifTrue: [^ false]. -- SFStream will give nil"
	state := converter saveStateOf: self.
	(next := self next) == nil ifTrue: [^ false].
	item = next ifTrue: [^ true].
	converter restoreStateOf: self with: state.
	^ false.

]

{ #category : #initialization }
MultiByteBinaryOrTextStream >> reset [

	super reset.
	isBinary ifNil: [isBinary := false].
	collection class == ByteArray ifTrue: ["Store as String and convert as needed."
		collection := collection asString.
		isBinary := true].

	self converter. "ensure that we have a converter."
]

{ #category : #'file in/out' }
MultiByteBinaryOrTextStream >> setConverterForCode [

	| current |
	current := converter saveStateOf: self.
	self position: 0.
	self binary.
	((self next: 3) =  #[239 187 191]) ifTrue: [
		self converter: UTF8TextConverter new
	] ifFalse: [
		self converter: MacRomanTextConverter new.
	].
	converter restoreStateOf: self with: current.
	self text.

]

{ #category : #'file in/out' }
MultiByteBinaryOrTextStream >> setEncoderForSourceCodeNamed: streamName [

	| l |
	l := streamName asLowercase.
	((l endsWith: 'cs') or: [
		(l endsWith: 'st') or: [
			(l endsWith: ('st.gz')) or: [
				(l endsWith: ('st.gz'))]]]) ifTrue: [
					self converter: MacRomanTextConverter new.
					^ self.
	].
	self converter: UTF8TextConverter new.

]

{ #category : #positioning }
MultiByteBinaryOrTextStream >> skipSeparators [

	[self atEnd] whileFalse: [
		self basicNext isSeparator ifFalse: [
			^ self position: self position - 1]]


]

{ #category : #accessing }
MultiByteBinaryOrTextStream >> skipSeparatorsAndPeekNext [

	"A special function to make nextChunk fast"
	| peek pos |
	[self atEnd] whileFalse: [
		pos := self position.
		(peek := self next) isSeparator ifFalse: [
			self position: pos.
			^ peek.
		].
	].

]

{ #category : #modes }
MultiByteBinaryOrTextStream >> text [
	isBinary := false

]

{ #category : #accessing }
MultiByteBinaryOrTextStream >> upTo: delim [ 

	| out ch |
	out := (String new: 1000) writeStream.
	self atEnd ifTrue: [^ ''].
	[(ch := self next) isNil] whileFalse: [
		(ch = delim) ifTrue: [
			^ out contents  "terminator is not doubled; we're done!"
		].
		out nextPut: ch.
	].
	^ out contents.

]

{ #category : #accessing }
MultiByteBinaryOrTextStream >> upToEnd [

	| newStream element newCollection |
	newCollection := self isBinary
				ifTrue: [ByteArray new: 100]
				ifFalse: [String new: 100].
	newStream := newCollection writeStream.
	[(element := self next) notNil]
		whileTrue: [newStream nextPut: element].
	^ newStream contents

]
