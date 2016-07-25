tests
testClass
	"self debug: #testClass"
	"The class of a compiled method is the class that contains it.
	A compiled method is shared."
	"methodClass could call 
		-> methodClassOrTrait"
	
	self assert: (Trait1>>#c) methodClass = Trait1.
	self assert: (Trait2>>#c) methodClass = Trait2.
	self assert: (MOPTestClassC>>#c) methodClass = MOPTestClassC.
	self assert: (MOPTestClassC>>#c2) methodClass = MOPTestClassC.
	self assert: (MOPTestClassD>>#c) methodClass = MOPTestClassD.
	self assert: (MOPTestClassD>>#c2) methodClass = MOPTestClassD.
	self assert: (MOPTestClassD>>#c3) methodClass = MOPTestClassD.
	self assert: (MOPTestClassA>>#c2) methodClass = MOPTestClassA.
	