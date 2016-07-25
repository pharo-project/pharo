tests - others
testAssociationAtNonexistantKeyExecutesFailBlock
	| methodSelector result error |
	methodSelector := #inexistant:method:larala:.
	error := #error.
	result := self class methodDict associationAt: methodSelector ifAbsent: [error].
	
	self assert: result = error.