tests
testClassModificationWithClassRemoved

	| aClass anEntry className |
	aClass := classFactory newClass.
	className := aClass name.
	aClass addInstVarNamed: #x.
	anEntry := monitor log head.
	
	aClass removeFromSystem.
	
	self assert: (anEntry content isKindOf: EpClassModification).
	self deny: (self class environment hasClassNamed: className).
	self apply: { anEntry }.
	self assert: (self class environment hasClassNamed: className).
	self assert: aClass instVarNames equals: #(x).
