tests
testDeleteIfAbsent
	| flag reference |
	flag := false.
	reference := filesystem / 'plonk'.

	reference ensureCreateFile.
	reference exists ifFalse: [self error].
	reference deleteIfAbsent: [flag := true].
	self deny: flag.
	
	reference exists ifTrue: [self error].
	reference deleteIfAbsent: [flag := true].
	self assert: flag.