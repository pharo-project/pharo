"
The central class to access the external file.  The interface of this object is similar to good old StandardFileStream, but internally it asks the converter, which is a sub-instance of TextConverter, and do the text conversion.

  It also combined the good old CrLfFileStream.  CrLfFileStream class>>new now returns an instance of MultiByteFileStream.

  There are several pitfalls:

  * You always have to be careful about the binary/text distinction.  In #text mode, it usually interpret the bytes.
  * A few file pointer operations treat the file as uninterpreted byte no matter what.  This means that if you use 'fileStream skip: -1', 'fileStream position: x', etc. in #text mode, the file position can be in the middle of multi byte character.  If you want to implement some function similar to #peek for example, call the saveStateOf: and restoreStateOf: methods to be able to get back to the original state.
  * #lineEndConvention: and #wantsLineEndConversion: (and #binary) can cause some puzzling situation because the inst var lineEndConvention and wantsLineEndConversion are mutated.  If you have any suggestions to clean up the protocol, please let me know.
"
Class {
	#name : #MultiByteFileStream,
	#superclass : #StandardFileStream,
	#instVars : [
		'converter',
		'lineEndConvention',
		'wantsLineEndConversion'
	],
	#classVars : [
		'Cr',
		'CrLf',
		'Lf',
		'LineEndDefault',
		'LineEndStrings',
		'LookAheadCount'
	],
	#category : #'DeprecatedFileStream-Base'
}

{ #category : #defaults }
MultiByteFileStream class >> defaultToCR [

	"MultiByteFileStream defaultToCR"
	LineEndDefault := #cr.

]

{ #category : #defaults }
MultiByteFileStream class >> defaultToCRLF [

	"MultiByteFileStream defaultToCRLF"
	LineEndDefault := #crlf.
]

{ #category : #defaults }
MultiByteFileStream class >> defaultToLF [

	"MultiByteFileStream defaultToLF"
	LineEndDefault := #lf.

]

{ #category : #defaults }
MultiByteFileStream class >> guessDefaultLineEndConvention [
	"Lets try to guess the line end convention from what we know about the
	path name delimiter from FileDirectory."
	
	FileSystem disk delimiter = $:
		ifTrue: [^ self defaultToCR].
		
	FileSystem disk delimiter = $/
		ifTrue: [^ (Smalltalk os isMacOSX or: [Smalltalk os isUnix])
				ifTrue: [ self defaultToLF]
				ifFalse: [ self defaultToCR]].
			
	FileSystem disk delimiter = $\
		ifTrue: [^ self defaultToCRLF].
		
	"in case we don't know"
	^ self defaultToCR
]

{ #category : #'class initialization' }
MultiByteFileStream class >> initialize [
	Cr := Character cr.
	Lf := Character lf.
	CrLf := String with: Cr with: Lf.
	LineEndStrings := Dictionary new.
	LineEndStrings at: #cr put: (String with: Character cr).
	LineEndStrings at: #lf put: (String with: Character lf).
	LineEndStrings at: #crlf put: (String with: Character cr with: Character lf).
	LookAheadCount := 2048.
	
	SessionManager default registerSystemClassNamed: self name.
	self startUp.

]

{ #category : #accessing }
MultiByteFileStream class >> lineEndDefault [
	"Answer the default line-ending convention that will be used by default, which was determined at start-up by looking at platform attributes."
	^ LineEndDefault

]

{ #category : #'instance creation' }
MultiByteFileStream class >> newFrom: aFileStream [

	| rw n |
	n := aFileStream name.
	rw := aFileStream isReadOnly not.
	aFileStream close.
	^self new open: n forWrite: rw.

]

{ #category : #'system startup' }
MultiByteFileStream class >> startUp [
	self guessDefaultLineEndConvention.

]

{ #category : #'system startup' }
MultiByteFileStream class >> startUp: resuming [
	self guessDefaultLineEndConvention.

]

{ #category : #remnant }
MultiByteFileStream >> accepts: aSymbol [

 	^ converter accepts: aSymbol.

]

{ #category : #modes }
MultiByteFileStream >> ascii [

	super ascii.
	self detectLineEndConvention.

]

{ #category : #'private - crlf' }
MultiByteFileStream >> bareNext [

	 ^ self converter nextFromStream: self.

]

{ #category : #'file in/out' }
MultiByteFileStream >> basicChunk [
	"If our buffer in collection contains an chunk with no embedded !'s, nor
	any non-ascii characters, return that.
	This presumes the source code encoding is unambiguously ascii-compatible"
	| bufferIX goodString |
	"Not possible if read buffering disabled"
	collection ifNil: [^nil].
	^ ((bufferIX := (collection indexOf: $! startingAt: position + 1) min: readLimit +1) > 0
			and: [bufferIX < collection size
					and: [(collection at: bufferIX + 1)
								~= $!
							and: [goodString := collection copyFrom: position + 1 to: bufferIX - 1.
								goodString isAsciiString]]])
		ifTrue: [ position := bufferIX.
				goodString]
]

{ #category : #private }
MultiByteFileStream >> basicNext: anInteger [

	^ super next: anInteger.

]

{ #category : #private }
MultiByteFileStream >> basicNext: n into: aString [

	^ super next: n into: aString.

]

{ #category : #private }
MultiByteFileStream >> basicNext: anInteger putAll: aCollection startingAt: startIndex [


        ^super next: anInteger putAll: aCollection startingAt: startIndex
]

{ #category : #private }
MultiByteFileStream >> basicNextInto: aString [

	^ super nextInto: aString.

]

{ #category : #private }
MultiByteFileStream >> basicNextPut: char [

	^ super nextPut: char.

]

{ #category : #private }
MultiByteFileStream >> basicNextPutAll: aString [

	^ super nextPutAll: aString.

]

{ #category : #private }
MultiByteFileStream >> basicPeek [

	^ super peek

]

{ #category : #private }
MultiByteFileStream >> basicPosition [

	^ super position.

]

{ #category : #private }
MultiByteFileStream >> basicPosition: pos [

	^ super position: pos.

]

{ #category : #private }
MultiByteFileStream >> basicReadInto: byteArray startingAt: startIndex count: count [

	^ super readInto: byteArray startingAt: startIndex count: count.

]

{ #category : #private }
MultiByteFileStream >> basicSetToEnd [

	^ super setToEnd.

]

{ #category : #private }
MultiByteFileStream >> basicSkip: n [

	^ super skip: n.

]

{ #category : #private }
MultiByteFileStream >> basicUpTo: delim [

	^ super upTo: delim.

]

{ #category : #private }
MultiByteFileStream >> basicVerbatim: aString [

	^ super verbatim: aString.

]

{ #category : #modes }
MultiByteFileStream >> binary [

	super binary.
	self lineEndConvention: nil
]

{ #category : #'private - crlf' }
MultiByteFileStream >> convertStringFromCr: aString [ 
	| inStream outStream |
	lineEndConvention ifNil: [^ aString].
	lineEndConvention == #cr ifTrue: [^ aString].
	lineEndConvention == #lf ifTrue: [^ aString copy replaceAll: Cr with: Lf].
	"lineEndConvention == #crlf"
	inStream := aString readStream.
	outStream :=  (String new: aString size) writeStream.
	[inStream atEnd]
		whileFalse: 
			[outStream nextPutAll: (inStream upTo: Cr).
			(inStream atEnd not or: [aString last = Cr])
				ifTrue: [outStream nextPutAll: CrLf]].
	^ outStream contents
]

{ #category : #'private - crlf' }
MultiByteFileStream >> convertStringToCr: aString [ 
	| inStream outStream |
	lineEndConvention ifNil: [^ aString].
	lineEndConvention == #cr ifTrue: [^ aString].
	lineEndConvention == #lf ifTrue: [^ aString copy replaceAll: Lf with: Cr].
	"lineEndConvention == #crlf"
	inStream := aString readStream.
	outStream := (String new: aString size) writeStream.
	[inStream atEnd]
		whileFalse: 
			[outStream nextPutAll: (inStream upTo: Cr).
			(inStream atEnd not or: [aString last = Cr])
				ifTrue: 
					[outStream nextPut: Cr.
					inStream peek = Lf ifTrue: [inStream next]]].
	^ outStream contents
]

{ #category : #accessing }
MultiByteFileStream >> converter [

	converter ifNil: [self converter: TextConverter defaultSystemConverter].
	^ converter

]

{ #category : #accessing }
MultiByteFileStream >> converter: aConverter [

	converter := aConverter.
	self installLineEndConventionInConverter

]

{ #category : #'private - crlf' }
MultiByteFileStream >> detectLineEndConvention [
	"Detect the line end convention used in this stream. The result may be either #cr, #lf or #crlf."
	| char numRead state |
	self isBinary ifTrue: [^ self error: 'Line end conventions are not used on binary streams'].
	wantsLineEndConversion == true ifFalse: [self lineEndConvention: nil.
					^lineEndConvention].
	self closed ifTrue: [self lineEndConvention: LineEndDefault.
					^lineEndConvention].

	"Default if nothing else found"
	numRead := 0.
	state := self converter saveStateOf: self.
	lineEndConvention := nil.
	[super atEnd not and: [numRead < LookAheadCount]]
		whileTrue: 
			[char := self next.
			char = Lf
				ifTrue: 
					[converter restoreStateOf: self with: state.
					self lineEndConvention: #lf.
					^lineEndConvention].
			char = Cr
				ifTrue: 
					[self peek = Lf
						ifTrue: [self lineEndConvention: #crlf]
						ifFalse: [self lineEndConvention: #cr].
					converter restoreStateOf: self with: state.
					^ lineEndConvention].
			numRead := numRead + 1].
	converter restoreStateOf: self with: state.
	self lineEndConvention: LineEndDefault.
	^ lineEndConvention
]

{ #category : #'private - crlf' }
MultiByteFileStream >> doConversion [

	^wantsLineEndConversion == true and: [ lineEndConvention notNil ]
]

{ #category : #accessing }
MultiByteFileStream >> fileInEncodingName: aString [

	self converter: (TextConverter newForEncoding: aString).
	super fileIn.

]

{ #category : #private }
MultiByteFileStream >> installLineEndConventionInConverter [

	converter ifNotNil: [
		converter installLineEndConvention: (
			(wantsLineEndConversion == true and: [ lineEndConvention notNil ]) "#doConversion is inlined here"
				ifTrue: [ LineEndStrings at: lineEndConvention ]
				ifFalse: [ nil ]) ]
]

{ #category : #accessing }
MultiByteFileStream >> lineEndConvention [

	^lineEndConvention
]

{ #category : #accessing }
MultiByteFileStream >> lineEndConvention: aSymbol [

	(lineEndConvention := aSymbol) ifNotNil: [wantsLineEndConversion := true].
	self installLineEndConventionInConverter
]

{ #category : #accessing }
MultiByteFileStream >> next [

	| char secondChar state |
	char := (converter ifNil: [ self converter ]) nextFromStream: self.
	(wantsLineEndConversion == true and: [ lineEndConvention notNil ]) "#doConversion is inlined here"
		 ifTrue: [
			char == Cr ifTrue: [
				state := converter saveStateOf: self.
				secondChar := self bareNext.
				secondChar ifNotNil: [
					secondChar == Lf ifFalse: [ converter restoreStateOf: self with: state ] ].
				^Cr ].
			char == Lf ifTrue: [
				^Cr ] ].
	^char.


]

{ #category : #accessing }
MultiByteFileStream >> next: anInteger [ 

	| multiString |
	self isBinary ifTrue: [^ super next: anInteger].
	multiString := String new: anInteger.
	1 to: anInteger do: [:index |
		| character |
		(character := self next)
			ifNotNil: [ multiString at: index put: character ]
			ifNil: [
				multiString := multiString copyFrom: 1 to: index - 1.
				(wantsLineEndConversion == true and: [ lineEndConvention notNil ]) "#doConversion is inlined here"
					ifFalse: [ ^multiString ].
				^self next: anInteger innerFor: multiString ] ].
	(wantsLineEndConversion == true and: [ lineEndConvention notNil ]) "#doConversion is inlined here"
		 ifFalse: [ ^multiString ].

	multiString := self next: anInteger innerFor: multiString.
	(multiString size = anInteger or: [self atEnd]) ifTrue: [ ^ multiString].
	^ multiString, (self next: anInteger - multiString size).

]

{ #category : #'private - crlf' }
MultiByteFileStream >> next: n innerFor: aString [

	| peekChar state |
	"if we just read a CR, and the next character is an LF, then skip the LF"
	aString size = 0 ifTrue: [^ aString].
	(aString last = Character cr) ifTrue: [
		state := converter saveStateOf: self.
		peekChar := self bareNext.		"super peek doesn't work because it relies on #next"
		(peekChar notNil and: [peekChar ~= Character lf]) ifTrue: [
			converter restoreStateOf: self with: state.
		].
	].
 
	^ aString withInternalLineEndings.

]

{ #category : #accessing }
MultiByteFileStream >> next: anInteger putAll: aCollection startingAt: startIndex [


        (self isBinary or: [ aCollection class == ByteArray ]) ifTrue: [
                ^super next: anInteger putAll: aCollection startingAt: startIndex ].
        ^self converter next: anInteger putAll: aCollection startingAt: startIndex toStream: self
]

{ #category : #'file in/out' }
MultiByteFileStream >> nextChunk [
	"Answer the contents of the receiver, up to the next terminator
	character. Doubled terminators indicate an embedded terminator
	character."
	self skipSeparators.
	^ self
		parseLangTagFor: (self basicChunk
				ifNil: [String
						new: 1000
						streamContents: [:stream | 
							| character state |
							[(character := self next) == nil
								or: [character == $!
										and: [state := converter saveStateOf: self.
											self next ~~ $!]]]
								whileFalse: [stream nextPut: character].
							character
								ifNotNil: [converter restoreStateOf: self with: state.]]])
]

{ #category : #accessing }
MultiByteFileStream >> nextDelimited: terminator [

	| out ch save |
	out := (String new: 1000) writeStream.
	self atEnd ifTrue: [^ ''].
	save := converter saveStateOf: self.

	self next = terminator ifFalse: [
		"absorb initial terminator"
		converter restoreStateOf: self with: save.
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
MultiByteFileStream >> nextMatchAll: aColl [

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

{ #category : #'file in/out' }
MultiByteFileStream >> nextPreamble [
	"Assuming that preamble part does not contain ]lang[ tag"
	self skipSeparators.
	^ self basicChunk
				ifNil: [String
						new: 1000
						streamContents: [:stream | 
							| character state |
							[(character := self next) == nil
								or: [character == $!
										and: [state := converter saveStateOf: self.
											self next ~~ $!]]]
								whileFalse: [stream nextPut: character].
							character
								ifNotNil: [converter restoreStateOf: self with: state.]]]
]

{ #category : #accessing }
MultiByteFileStream >> nextPut: aCharacter [
	aCharacter isInteger 
		ifTrue: [ ^ super nextPut: aCharacter ].
	(wantsLineEndConversion == true and: [ lineEndConvention notNil ]) "#doConversion is inlined here"
		 ifTrue: [
			aCharacter = Cr
				ifTrue: [ converter nextPutAll: (LineEndStrings at: lineEndConvention) toStream: self ]
				ifFalse: [ converter nextPut: aCharacter toStream: self ].
			^aCharacter ].
	^ self converter nextPut: aCharacter toStream: self

]

{ #category : #accessing }
MultiByteFileStream >> nextPutAll: aCollection [
	^ (self isBinary or: [ aCollection class == ByteArray ])
		ifTrue: [ super nextPutAll: aCollection ]
		ifFalse: [ self converter nextPutAll: aCollection toStream: self ]
]

{ #category : #'open/close' }
MultiByteFileStream >> open: fileName forWrite: writeMode [ 
	| result |

	(writeMode and: [SessionManager default currentSession isReadOnlyAccessMode]) 
		ifTrue: [ ((CannotDeleteFileException fileName: fileName)
			messageText: 'Attempt to open file ' , fileName, ' as writable on read-only filesystem') signal. ].

	result := super open: fileName forWrite: writeMode.
	result ifNotNil: [
			converter ifNil: [self converter: UTF8TextConverter new].
			lineEndConvention ifNil: [ self detectLineEndConvention ]
	].
	^result
]

{ #category : #accessing }
MultiByteFileStream >> peek [
	"Answer what would be returned if the message next were sent to the receiver. If the receiver is at the end, answer nil.  "

	| next save |
	self atEnd ifTrue: [^ nil].
	save := converter saveStateOf: self.
	next := self next.
	converter restoreStateOf: self with: save.
	^ next.


]

{ #category : #accessing }
MultiByteFileStream >> peekFor: item [ 

	| next state |
	state := converter saveStateOf: self.
	(next := self next) == nil ifTrue: [^ false].
	item = next ifTrue: [^ true].
	converter restoreStateOf: self with: state.
	^ false.

]

{ #category : #accessing }
MultiByteFileStream >> readInto: buffer startingAt: offset count: requestedCount [
	"Read up to requestedCount elements into the given buffer starting at offset.
	Return the number of elements actually read.
	If I am binary or if buffer is a ByteArray, I skip decoding. Yes this is weird.
	This is a necessarily inefficient implementation, reading and decoding characters one by one."

	(self isBinary or: [ buffer class == ByteArray ])
		ifTrue: [ ^ super readInto: buffer startingAt: offset count: requestedCount ].
	0 to: requestedCount - 1 do: [ :count | 
		| element |
		(element := self next) ifNil: [ ^ count ].
		buffer at: offset + count put: element ].
	^ requestedCount
]

{ #category : #private }
MultiByteFileStream >> requestDropStream: dropIndex [
	"Needs to install proper converter"
	
	| result |
	result := super requestDropStream: dropIndex.
	result ifNotNil: [
		converter ifNil: [self converter: UTF8TextConverter new].
		lineEndConvention ifNil: [ self detectLineEndConvention] ].
	^result
]

{ #category : #initialization }
MultiByteFileStream >> reset [

	super reset.
	converter ifNil: [
		self converter: UTF8TextConverter new.
	].

]

{ #category : #private }
MultiByteFileStream >> setConverterForCode [

	| current |
	SourceFiles changesFileStream ifNotNil: [ :aStream |
		self fullName = aStream fullName ifTrue: [ ^ self ] ].
	current := self converter saveStateOf: self.
	self position: 0.
	self binary.
	((self next: 3) = #[ 16rEF 16rBB 16rBF ]) ifTrue: [
		self converter: UTF8TextConverter new
	] ifFalse: [
		self converter: MacRomanTextConverter new.
	].
	converter restoreStateOf: self with: current.
	self text.

]

{ #category : #positioning }
MultiByteFileStream >> skipSeparators [

	| state character |
	[ 
		state := converter saveStateOf: self.
		(character := self next) 
			ifNil: [ false ]
			ifNotNil: [ character isSeparator ] ] whileTrue.
	character ifNotNil: [ 
		converter restoreStateOf: self with: state ]
]

{ #category : #accessing }
MultiByteFileStream >> skipSeparatorsAndPeekNext [
	"Same as #skipSeparators, but returns the next character after the separators if such exists."

	| state character |
	[ 
		state := converter saveStateOf: self.
		(character := self next) 
			ifNil: [ false ]
			ifNotNil: [ character isSeparator ] ] whileTrue.
	character ifNotNil: [ 
		converter restoreStateOf: self with: state.
		^character ].
	
]

{ #category : #accessing }
MultiByteFileStream >> upTo: delimiter [

	^self collectionSpecies new: 1000 streamContents: [ :stream |
		| ch |
		[ (ch := self next) == nil or: [ ch = delimiter ] ] 
			whileFalse: [ stream nextPut: ch ] ]

]

{ #category : #accessing }
MultiByteFileStream >> upToAnyOf: delimiters do: aBlock [

	^self collectionSpecies new: 1000 streamContents: [ :stream |
		| ch |
		[ (ch := self next) == nil or: [ (delimiters includes: ch) and: [aBlock value: ch. true] ] ] 
			whileFalse: [ stream nextPut: ch ] ]
]

{ #category : #accessing }
MultiByteFileStream >> upToEnd [

	^self collectionSpecies
		new: self size - self position
		streamContents: [ :stream |
			| element |
			[ (element := self next) == nil ] whileFalse: [
				stream nextPut: element ] ]

]

{ #category : #accessing }
MultiByteFileStream >> upToPosition: anInteger [
	"Answer a subcollection containing items starting from the current position and ending including the given position. Usefully different to #next: in that positions measure *bytes* from the file, where #next: wants to measure *characters*."
	^self collectionSpecies new: 1000 streamContents: [ :stream |
		| ch |
		[ (ch := self next) == nil or: [ position > anInteger ] ] 
			whileFalse: [ stream nextPut: ch ] ]
]

{ #category : #'private - crlf' }
MultiByteFileStream >> wantsLineEndConversion [

	^wantsLineEndConversion == true

]

{ #category : #remnant }
MultiByteFileStream >> wantsLineEndConversion: aBoolean [
	
	wantsLineEndConversion :=  aBoolean.

	lineEndConvention ifNil: [ self detectLineEndConvention ]. 
]
