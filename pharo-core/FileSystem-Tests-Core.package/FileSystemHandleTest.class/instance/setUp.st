running
setUp
	filesystem := self createFileSystem.
	reference := filesystem * 'plonk'.
	handle := reference openWritable: true