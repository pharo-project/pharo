public
allGTExamples
	" self allGTExamples "
	| examples |
	examples := OrderedCollection new.
	self allGTExamplesTo: examples.
	^ examples