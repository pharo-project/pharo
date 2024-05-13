Extension { #name : 'SequenceableCollection' }

{ #category : '*Collections-Streams' }
SequenceableCollection class >> new: newSize streamContents: blockWithArg [
	"A variant of #streamContents: where the initial or even final size is given to optimize memory consumption"

	"(Array new: 3 streamContents: [ :out | 3 timesRepeat: [ out nextPut: 42 ] ]) >>> #(42 42 42)"

	| stream |
	stream := WriteStream on: (self streamSpecies new: newSize).
	blockWithArg value: stream.
	"If the write position of stream is at the end of the internal buffer of stream (originalContents),
	we can return it directly instead of making a copy as contents would do"
	^ stream position = stream originalContents size
		ifTrue: [ stream originalContents ]
		ifFalse: [ stream contents ]
]

{ #category : '*Collections-Streams' }
SequenceableCollection >> readStream [
	^ ReadStream on: self
]

{ #category : '*Collections-Streams' }
SequenceableCollection class >> streamContents: blockWithArg limitedTo: sizeLimit [
	"A variant of #streamContents: with a strict size limit"

	"(String streamContents: [:s | 10 timesRepeat: [s nextPutAll: 'foo']] limitedTo: 9) >>> 'foofoofoo'"

	| stream |
	stream := LimitedWriteStream
		on: (self streamSpecies new: (100 min: sizeLimit))
		limit: sizeLimit
		limitBlock: [ ^ stream contents ].
	blockWithArg value: stream.
	^ stream contents
]

{ #category : '*Collections-Streams' }
SequenceableCollection >> writeStream [
	^ WriteStream on: self
]

{ #category : '*Collections-Streams' }
SequenceableCollection >> writeStreamDo: aBlock [
	"Evaluates the argument with the write stream of the collection. Answers the result."

	"(#() writeStreamDo: [ :stream | stream nextPut: '4'; space; nextPutAll: '34'. stream contents ]) >>> {'4'. Character space. $3. $4}"

	^ aBlock value: self writeStream
]
