tests
testForClassLocals
	| model locals temps |
	temps := #('loc1' 'x2' 'bar' 'var' ) collect: 
		[ :each | 
		NECLocalEntry 
			contents: each
			type: #local ].
	model := NECUntypedModel 
		class: NECTestClass
		temporaries: temps.
	model loadEntries.
	locals := model entriesOfType: #local.
	self assert: locals size == 4.
	self assert: (locals includes: 'loc1').
	self assert: (locals includes: 'x2').
	self assert: (locals includes: 'bar').
	self assert: (locals includes: 'var').
	model narrowWith: 'l'.
	locals := model entriesOfType: #local.
	self assert: locals size == 1.
	self assert: (locals includes: 'loc1')