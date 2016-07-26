lint
fileBlocks
	| file |
	^
	[file := 'asdf' asFilename readStream.
	file contents] ensure: [file close]