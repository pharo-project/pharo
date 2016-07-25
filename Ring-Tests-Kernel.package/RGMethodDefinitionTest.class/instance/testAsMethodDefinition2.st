testing
testAsMethodDefinition2
	"self debug: #testAsMethodDefinition2"
	
	| newMethod |
	newMethod := (Object >> #printOn:) asRingDefinition.
	self assert:  newMethod protocol = 'printing'.
	
	newMethod := (TCloneTest>>#testCopyEmpty) asRingDefinition.
	self assert:  newMethod protocol = 'tests - copy - clone'.

	newMethod := (ArrayTest>>#testCopyEmpty) asRingDefinition.
	self assert:  newMethod protocol = 'tests - copy - clone'.
	
	