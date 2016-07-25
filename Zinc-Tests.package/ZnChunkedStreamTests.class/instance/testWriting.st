testing
testWriting
	| chunks result reference |
	chunks := #( 'ABC' 'DEF' 'GHI' 'JKL' ).
	result := String streamContents: [ :out |
		| stream |
		stream := ZnChunkedWriteStream on: out.
		chunks do: [ :each | stream nextPutAll: each ].
		stream finish ].
	reference := String streamContents: [ :out |
		self nextPutAll: (String new join: chunks) on: out chunked: 3 ].
	self assert: result equals: reference