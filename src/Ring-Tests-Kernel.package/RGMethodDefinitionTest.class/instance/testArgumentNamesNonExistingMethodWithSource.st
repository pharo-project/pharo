test - accessing source
testArgumentNamesNonExistingMethodWithSource
	"accessing ring methods arguments for passive ring definition created with source"

	| newMethod newClass source |
	source := 'class: aClass selector: aSelector
              "new passive method"'.
	newClass := RGClassDefinition named: #RGMethodDefinition.
	newMethod := (RGMethodDefinition named: #class:selector:) parent: newClass.
	newMethod sourceCode: source.
	self assert: newMethod isPassive.
	self assertCollection: newMethod argumentNames hasSameElements: #( #aClass #aSelector).
