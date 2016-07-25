testing
testLineTooLong
	| input reader |
	input := (String new: 128) atAllPut: $X.
	reader := (ZnLineReader on: input readStream) limit: 64.
	self should: [ reader nextLine ] raise: ZnLineTooLong  