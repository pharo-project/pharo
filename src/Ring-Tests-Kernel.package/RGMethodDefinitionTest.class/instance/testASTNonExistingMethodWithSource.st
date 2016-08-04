test - accessing source
testASTNonExistingMethodWithSource
	"accessing ring methods ast for passive ring definition created with source"

	| newMethod newClass source |
	source := 'foo
   ^ self'.
	newClass := RGClassDefinition named: #RGMethodDefinition.
	newMethod := (RGMethodDefinition named: #foo) parent: newClass.
	newMethod sourceCode: source.
	self assert: newMethod isPassive.
	self assert: newMethod ast equals: (RBParser parseMethod: source)
