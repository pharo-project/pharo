tests
testWithExtentionAddsExtension
	| path result |
	path := Path * 'plonk'.
	result := path withExtension: 'griffle'.
	self assert: result basename equals: 'plonk.griffle'