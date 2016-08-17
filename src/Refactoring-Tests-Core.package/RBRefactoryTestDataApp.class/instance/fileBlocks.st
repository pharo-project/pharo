lint
fileBlocks
	| file |
	^
	[file := 'asdf' asFileReference readStream.
	file contents] ensure: [file close]