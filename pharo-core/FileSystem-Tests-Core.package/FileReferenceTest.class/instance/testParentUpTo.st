tests
testParentUpTo
	| base a b c |
	[ 
		base := filesystem workingDirectory.
		(base / 'testParentUpTo') ensureCreateDirectory.
		a := (base / 'testParentUpTo' / 'A') ensureCreateDirectory.
		b := (base / 'testParentUpTo' / 'A' / 'B') ensureCreateDirectory.
		c := (base / 'testParentUpTo' / 'A' / 'B' / 'C') ensureCreateDirectory.
		self assert: b equals: (c parentUpTo: 'B').
		self assert: a equals: (c parentUpTo: 'A').
		self assert: (base / 'testParentUpTo') equals: (c parentUpTo: 'testParentUpTo').
		self assert: base equals: (c parentUpTo: 'notAParent') 
	] ensure: [ 
		(base / 'testParentUpTo') ensureDeleteAll 
	]