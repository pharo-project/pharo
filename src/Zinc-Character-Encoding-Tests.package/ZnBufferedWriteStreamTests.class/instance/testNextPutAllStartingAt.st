testing
testNextPutAllStartingAt
	| string |
	string := String streamContents: [ :stringStream | 
		ZnBufferedWriteStream on: stringStream do: [ : bufferedStream |
			bufferedStream sizeBuffer: 8.
			bufferedStream next: 5 putAll: '--012345--' startingAt: 3.
			bufferedStream next: 5 putAll: '0123456789XX' startingAt: 6. 
			bufferedStream next: 5 putAll: '--012345--' startingAt: 3.
			bufferedStream next: 5 putAll: '0123456789XX' startingAt: 6.] ].
	self assert: string equals: '01234567890123456789'