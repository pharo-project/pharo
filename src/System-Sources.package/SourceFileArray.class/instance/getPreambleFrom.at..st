public - string reading
getPreambleFrom: aFileStream at: position
	"To read preamble of method we need read back characters until $!.
	But given aFileStream can have UTF8 encoding which make it a bit tricky to scan stream in reverse order.
	First we need to move back in stream using byte reading without encoding. It performed by basicNext.
	Next we still could not read exact number of characters because (position-startIndex) is not characters number in case of UTF8.
	So at the end we use another loop to read encoded characters step by step until stream will be at original position"
	| startIndex |
	startIndex := position.
	[startIndex >= 0 and: [aFileStream position: startIndex.
	aFileStream basicNext ~~ $!] ] whileTrue: [ startIndex := startIndex - 1 ].

	^String streamContents: [ :result | 
		[aFileStream position <= position] whileTrue: [result nextPut: aFileStream next]]
