tests
testOrigin
	"self debug: #testClass"
	"The origin of a compiledMethod is its defining class or trait."

	| aliasedMethod |

	self assert: (MOPTestClassC>>#c) origin = MOPTestClassC.
	self assert: (MOPTestClassA>>#c) origin = Trait3.
	self assert: (Trait3>>#c2) origin = Trait2.
	self assert: (MOPTestClassA>>#c2) origin = Trait2.
	self assert: (MOPTestClassB>>#c) origin = Trait1.
	self assert: (MOPTestClassD>>#c3) origin = Trait2.
	self assert: (MOPTestClassD>>#c2) origin = Trait2.
	
	aliasedMethod := MOPTestClassD >>#c3.
	self assert: aliasedMethod origin == aliasedMethod originMethod methodClass.