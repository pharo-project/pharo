testing
testReadIntoStartingAtCountFromStreamWithOffset
	| input encoder bytes readStream string read |
	encoder := ZnUTF8Encoder new.
	input := '_élève_'.
	bytes := encoder encodeString: input.
	readStream := bytes readStream.
	readStream next.
	string := String new: 7 withAll: $_.
	read := encoder readInto: string startingAt: 2 count: 5 fromStream: readStream.
	self assert: string equals: input.
	self assert: read equals: 5.
	input := '_Français_'.
	bytes := encoder encodeString: input.
	readStream := bytes readStream.
	readStream next.
	string := String new: 10 withAll: $_.
	read := encoder readInto: string startingAt: 2 count: 8 fromStream: readStream.
	self assert: string equals: input.
	self assert: read equals: 8