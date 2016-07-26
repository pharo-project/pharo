image reading/writing
formsFromStream: stream 
	| reader |
	reader := self new on: stream reset.
	Cursor read showWhile: 
		[ reader allImages.
		reader close ].
	^ reader