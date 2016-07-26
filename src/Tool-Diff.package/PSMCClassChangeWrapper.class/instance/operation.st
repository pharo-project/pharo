accessing
operation
	"Answer the patch operation for the class itself or nil if none."

	^ self model detect: [ :i | i targetClassName = self item and: [ i definition isClassDefinition ] ] ifNone: [ nil ]