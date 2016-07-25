tests
testComment
	| change |
	change := changes comment: 'Some Comment' in: self class.
	self assert: change changeClassName = self class name.
	self assert: change changeClass = self class.
	self assert: change isMeta not.
	self assert: change comment = 'Some Comment'.
	self universalTestFor: change