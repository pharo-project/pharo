Extension { #name : #ZnCharacterReadStream }

{ #category : #'*System-Changes' }
ZnCharacterReadStream >> basicNextChunk [
	"Answer the contents of the receiver, up to the next terminator character. Doubled terminators indicate an embedded terminator character."

	| terminator out ch |
	terminator := $!.
	out := (String new: 1000) writeStream.
	self skipSeparators.
	[ (ch := self next) isNil ]
		whileFalse: [ 
			ch == terminator
				ifTrue: [ 
					self peek == terminator
						ifTrue: [ self next	"skip doubled terminator" ]
						ifFalse: [ ^ out contents	"terminator is not doubled; we're done!" ] ].
			out nextPut: ch ].
	^ out contents
]

{ #category : #'*System-Changes' }
ZnCharacterReadStream >> decodeString: string andRuns: runsRaw [
	| strm runLength runValues newString index |
	strm := ReadStream on: runsRaw from: 1 to: runsRaw size.
	(strm peekFor: $()
		ifFalse: [ ^ nil ].
	runLength := OrderedCollection new.
	[ 
	strm skipSeparators.
	strm peekFor: $) ] whileFalse: [ runLength add: (Number readFrom: strm) ].
	runValues := OrderedCollection new.
	[ strm atEnd ]
		whileFalse: [ 
			runValues add: (Number readFrom: strm).
			strm next ].
	newString := WideString new: string size.
	index := 1.
	runLength
		with: runValues
		do: [ :length :leadingChar | 
			index to: index + length - 1 do: [ :pos | newString at: pos put: (Character leadingChar: leadingChar code: (string at: pos) charCode) ].
			index := index + length ].
	^ newString
]

{ #category : #'*System-Changes' }
ZnCharacterReadStream >> nextChunk [
	"Answer the contents of the receiver, up to the next terminator character. Doubled terminators indicate an embedded terminator character."

	| terminator out ch |
	terminator := $!.
	out := (String new: 1000) writeStream.
	self skipSeparators.
	[ (ch := self next) isNil ]
		whileFalse: [ 
			ch == terminator
				ifTrue: [ 
					self peek == terminator
						ifTrue: [ self next	"skip doubled terminator" ]
						ifFalse: [ ^ self parseLangTagFor: out contents	"terminator is not doubled; we're done!" ] ].
			out nextPut: ch ].
	^ self parseLangTagFor: out contents
]

{ #category : #'*System-Changes' }
ZnCharacterReadStream >> nextPreamble [
	"Assuming that preamble part does not contain ]lang[ tag"
	| terminator out ch |
	terminator := $!.
	out := (String new: 1000) writeStream.
	self skipSeparators.
	[ (ch := self next) isNil ]
		whileFalse: [ 
			ch == terminator
				ifTrue: [ 
					self peek == terminator
						ifTrue: [ self next	"skip doubled terminator" ]
						ifFalse: [ ^out contents	"terminator is not doubled; we're done!" ] ].
			out nextPut: ch ].
	^ out contents
]

{ #category : #'*System-Changes' }
ZnCharacterReadStream >> parseLangTagFor: aString [

	| string peek runsRaw pos |
	string := aString.
	"Test for ]lang[ tag"
	pos := self position.
	peek := self skipSeparatorsAndPeekNext.
	peek = $] ifFalse: [self position: pos. ^ string].  "no tag"
	(self upTo: $[) = ']lang' ifTrue: [
		runsRaw := self basicNextChunk.
		string := self decodeString: aString andRuns: runsRaw
	] ifFalse: [
		self position: pos
	].
	^ string.

]

{ #category : #'*System-Changes' }
ZnCharacterReadStream >> skipStyleChunk [
	"Get to the start of the next chunk that is not a style for the previous chunk"

	| pos |
	pos := self position.
	self skipSeparators.
	self peek == $] 
		ifTrue: [(self upTo: $[) = ']text' 	"old -- no longer needed"
				"now positioned past the open bracket"
			ifFalse: [self nextChunk]]	"absorb ]style[ and its whole chunk"
				
		ifFalse: [self position: pos]	"leave untouched"

]
