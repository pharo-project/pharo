tests-fuelAfterMaterialization
testExecuteAfterMaterialization

	| anObject result aClass |
	aClass := 
		self newClass
			addInstVarNamed: 'a';
			duringTestCompileSilently: 'fuelAfterMaterialization a := #A';
			yourself.
	anObject := aClass new.

	result := self resultOfSerializeAndMaterialize: anObject.

	self assert: #A = (result instVarNamed: 'a').
