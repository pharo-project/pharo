ston persistence
fromSton: stonReader
	| array stream |
	array := Array new: 2.
	stream := array writeStream.
	stonReader parseListDo: [ :each :index |
		stream nextPut: each ].

	^ self 
		globalName: array first
		localName: array second 