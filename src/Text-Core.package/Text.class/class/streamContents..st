instance creation
streamContents: blockWithArg 
	| stream |
	stream := TextStream on: (self new: 400).
	blockWithArg value: stream.
	^ stream contents