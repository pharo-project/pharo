testing
testReadIntoStartingAtCountFromStreamWithOffset
	| input encoder bytes readStream string |
	encoder := ZnUTF8Encoder new.
	input := '_élève_'.
	bytes := encoder encodeString: input.
	readStream := bytes readStream.
	readStream next.
	string := String new: 7 withAll: $_.
	encoder readInto: string startingAt: 2 count: 5 fromStream: readStream.
	self assert: string equals: input.
	input := '_Français_'.
	bytes := encoder encodeString: input.
	readStream := bytes readStream.
	readStream next.
	string := String new: 10 withAll: $_.
	encoder readInto: string startingAt: 2 count: 8 fromStream: readStream.
	self assert: string equals: input