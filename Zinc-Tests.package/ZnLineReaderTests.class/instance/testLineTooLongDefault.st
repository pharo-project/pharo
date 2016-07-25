testing
testLineTooLongDefault
	| input reader |
	input := (String new: ZnConstants maximumLineLength + 1) atAllPut: $X.
	reader := ZnLineReader on: input readStream.
	self should: [ reader nextLine ] raise: ZnLineTooLong  