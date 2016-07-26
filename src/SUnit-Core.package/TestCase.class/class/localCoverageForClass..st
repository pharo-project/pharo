coverage
localCoverageForClass: cls
	
	| definedMethods testedMethods untestedMethods |
	definedMethods := cls selectors.
	"It happens for IdentityBag / IdentityBagTest"
	definedMethods size = 0
		ifTrue: [^ {0. Set new}].

	testedMethods := 
		self methods inject: Set new into: 
							[:sums :cm | sums union: cm messages].
					
	"testedMethods contains all the methods send in test methods, which probably contains methods that have nothign to do with collection"
	testedMethods := testedMethods reject: [:sel | (definedMethods includes: sel) not].

	untestedMethods := definedMethods select: [:selector | (testedMethods includes: selector) not].

	^ { (testedMethods size * 100 / definedMethods size) asFloat . untestedMethods}
