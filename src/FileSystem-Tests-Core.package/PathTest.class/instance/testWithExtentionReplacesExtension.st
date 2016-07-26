tests
testWithExtentionReplacesExtension
	| path result |
	path := Path * 'plonk.griffle'.
	result := path withExtension: 'griffle'.
	self assert: result basename equals: 'plonk.griffle'