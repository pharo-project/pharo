tests
testResetUnwind
	"Just like close, just using reset"
	| generator doEnsure notEnsure |
	doEnsure := notEnsure := 0.
	[ generator := Generator on: [ :g |
		[ g yield: 1; yield: 2 ]
			ensure: [ doEnsure := doEnsure + 1 ] ].
	self assert: doEnsure = 0; assert: notEnsure = 0.
	self assert: generator peek = 1.
	self assert: doEnsure = 0; assert: notEnsure = 0.
	generator reset.
	self assert: doEnsure = 1; assert: notEnsure = 0 ]
		ensure: [ notEnsure := notEnsure + 1 ].
	self assert: doEnsure = 1; assert: notEnsure = 1