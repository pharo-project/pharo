tests - others
testKeyAtNonexistantValueExecutesFailBlock
	| methodSelector result error |
	methodSelector := #inexistant:method:larala:.
	error := #error.
	result := self class methodDict keyAtValue: self ifAbsent: [error].
	
	self assert: result = error.