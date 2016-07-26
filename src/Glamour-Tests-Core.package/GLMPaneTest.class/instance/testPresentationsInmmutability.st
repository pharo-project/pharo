tests
testPresentationsInmmutability
	| aPane presentations |
	aPane := GLMPane in: 1.
	self assert: aPane presentations isEmpty.
	presentations := aPane presentations.
	presentations add: 1.
	self assert: presentations size equals: 1.
	self assert: aPane presentations isEmpty