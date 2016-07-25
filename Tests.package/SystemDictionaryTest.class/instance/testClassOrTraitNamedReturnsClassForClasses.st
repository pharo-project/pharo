tests
testClassOrTraitNamedReturnsClassForClasses
	"self debug: #testClassOrTraitNamedReturnsClassForClasses"
	self assert: Object ==  (Smalltalk globals classOrTraitNamed: 'Object').
	self assert: Object ==  (Smalltalk globals classOrTraitNamed: #Object).