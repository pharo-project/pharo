accessing
findAllPaths: aBlock 
	| paths |
	paths := OrderedCollection new.
	foundBlock := [:path | paths add: path].
	testBlock := aBlock.
	self find.
	^paths