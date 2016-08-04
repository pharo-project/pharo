test - accessing source
testArgumentNamesNonExistingMethodWithNoSource
	"accessing ring methods arguments for passive ring definition created with source"

	| newMethod newClass |
	newClass := RGClassDefinition named: #RGMethodDefinition.
	newMethod := (RGMethodDefinition named: #foo:) parent: newClass.
	self assert: newMethod isPassive.
	self assert: newMethod argumentNames isNil.
