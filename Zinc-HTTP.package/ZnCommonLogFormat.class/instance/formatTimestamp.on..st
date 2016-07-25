private
formatTimestamp: timestamp on: stream
	stream nextPut: $[.
	timestamp dayOfMonth printOn: stream base: 10 length: 2 padded: true.
	stream nextPut: $/.
	stream << timestamp monthAbbreviation.
	stream nextPut: $/.
	timestamp year printOn: stream.
	stream << $:.
	timestamp printHMSOn: stream.
	stream space.
	stream nextPut: (timestamp offset positive ifTrue: [ $+ ] ifFalse: [ $- ]).
	timestamp offset hours abs printOn: stream base: 10 length: 2 padded: true.
	stream nextPut: $:.
	timestamp offset minutes abs printOn: stream base: 10 length: 2 padded: true.
	stream nextPut: $]