test - accessing source
testMessagesNonExistingMethodWithSource
	"accessing ring sent messages for passive method created with source"

	| newMethod newClass source |
	source := 'class: aClass selector: aSelector
              ^ self new; yourself'.
	newClass := RGClassDefinition named: #RGMethodDefinition.
	newMethod := (RGMethodDefinition named: #class:selector:) parent: newClass.
	newMethod sourceCode: source.
	self assert: newMethod isPassive.
	self assertCollection: newMethod messages hasSameElements: #( #new #yourself).
