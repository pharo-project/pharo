test - accessing source
testASTNonExistingMethodWithNoSource
	"accessing ring methods ast for passive ring definition with no source"

	| newMethod newClass |
	newClass := RGClassDefinition named: #RGMethodDefinition.
	newMethod := (RGMethodDefinition named: #foo) parent: newClass.
	self assert: newMethod isPassive.
	self assert: newMethod ast isNil.