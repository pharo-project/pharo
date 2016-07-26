tests
testClassOrTraitNamedReturnsNilForGlobals
	"self debug: #testClassOrTraitNamedReturnsNilForGlobals"
	self assert: nil = (Smalltalk globals classOrTraitNamed: 'Undeclared').
	self assert: nil = (Smalltalk globals classOrTraitNamed: #Undeclared).