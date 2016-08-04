test - accessing source
testMessagesNonExistingMethodWithNoSource
	"accessing ring sent messages for passive method with no source"

	| newMethod newClass |
	newClass := RGClassDefinition named: #RGMethodDefinition.
	newMethod := (RGMethodDefinition named: #class:selector:) parent: newClass.
	self assert: newMethod isPassive.
	self assert: newMethod messages equals: #()
