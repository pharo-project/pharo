Extension { #name : #String }

{ #category : #'*Deprecated90' }
String >> convertFromEncoding: encodingName [
	^self convertFromWithConverter: (TextConverter newForEncoding: encodingName)
]

{ #category : #'*Deprecated90' }
String >> convertFromWithConverter: converter [

	| readStream c |
	readStream := self readStream.
	^ self species new: self size streamContents: [ :writeStream|
		converter ifNil: [^ self].
		[readStream atEnd] whileFalse: [
			c := converter nextFromStream: readStream.
			c
				ifNotNil: [writeStream nextPut: c] 
				ifNil: [^ writeStream contents]]].

]

{ #category : #'*Deprecated90' }
String >> convertToEncoding: encodingName [
	^self convertToWithConverter: (TextConverter newForEncoding: encodingName).
]

{ #category : #'*Deprecated90' }
String >> convertToWithConverter: converter [ 
	converter
		ifNil: [^ self].
	^ String new: self size streamContents: [:writeStream | 
		converter 
			nextPutAll: self toStream: writeStream;
			emitSequenceToResetStateIfNeededOn: writeStream]
]

{ #category : #'*Deprecated90' }
String >> findSubstring: key in: body startingAt: start matchTable: matchTable [
	"Answer the index in the string body at which the substring key first occurs, at or beyond start.  The match is determined using matchTable, which can be used to effect, eg, case-insensitive matches.  If no match is found, zero will be returned."
	| index c1 c2 |
	matchTable == nil ifTrue: [
		key size = 0 ifTrue: [^ 0].
		start to: body size - key size + 1 do:
			[:startIndex |
			index := 1.
				[(body at: startIndex+index-1)
					= (key at: index)]
					whileTrue:
					[index = key size ifTrue: [^ startIndex].
					index := index+1]].
		^ 0
	].

	key size = 0 ifTrue: [^ 0].
	start to: body size - key size + 1 do:
		[:startIndex |
		index := 1.
		[c1 := body at: startIndex+index-1.
		c2 := key at: index.
		((c1 leadingChar = 0 and: [ c1 asciiValue < matchTable size ]) 
			ifTrue: [ matchTable at: c1 asciiValue + 1 ]
			ifFalse: [ c1 asciiValue + 1 ]) = 
			((c2 leadingChar = 0 and: [ c2 asciiValue < matchTable size ])
				ifTrue: [ matchTable at: c2 asciiValue + 1 ]
				ifFalse: [c2 asciiValue + 1 ]) ]
			whileTrue:
				[index = key size ifTrue: [^ startIndex].
				index := index+1]].
	^ 0
]

{ #category : #'*Deprecated90' }
String >> leadingCharRunLengthAt: index [

	| leadingChar |
	leadingChar := (self at: index) leadingChar.
	index to: self size do: [:i |
		(self at: i) leadingChar ~= leadingChar ifTrue: [^ i - index].
	].
	^ self size - index + 1.

]
