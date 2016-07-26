tests - others
testAtNonexistantKeyExecutesFailBlock
	| methodSelector result error |
	methodSelector := #inexistant:method:larala:.
	error := #error.
	result := self class methodDict at: methodSelector ifAbsent: [error].
	
	self assert: result = error.