tests
testEnsureClosed
	filesystem := self createFileSystem.
	reference := filesystem * 'plonk'.
	handle := reference openWritable: true.
	handle ensureClosed.
	self deny: handle isOpen.
	handle ensureClosed.
	reference ensureDelete.
	handle reference exists
		ifTrue: [ self error ].
	handle ensureClosed