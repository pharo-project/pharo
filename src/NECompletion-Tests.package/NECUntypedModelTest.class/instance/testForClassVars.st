tests
testForClassVars
	| model classVars |
	model := NECUntypedModel 
		class: NECTestClass
		temporaries: OrderedCollection new.
	model loadEntries.
	classVars := model entriesOfType: #classVar.
	self assert: classVars size >= 2.
	self assert: (classVars includes: 'SuperClassVar').
	self assert: (classVars includes: 'ClassVar')