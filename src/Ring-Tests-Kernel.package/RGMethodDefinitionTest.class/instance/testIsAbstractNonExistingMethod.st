test - accessing source
testIsAbstractNonExistingMethod
	"testing non existing ring method isAbstract"

	| newMethod newClass |
	newClass := RGClassDefinition named: #RGMethodDefinition.
	newMethod := (RGMethodDefinition named: #foo:) parent: newClass.
	self assert: newMethod isPassive.
	self deny: newMethod isAbstract.
