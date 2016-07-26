coverage
coverageForClass: cls until: aRootClass
	"returns the test coverage of all the methods included inherited ones but stopping at aRootClass included"
	
	| definedMethods testedMethods untestedMethods |
	definedMethods := cls allSelectorsAboveUntil: aRootClass.
	definedMethods size = 0
		ifTrue: [^ {0. Set new}].
	testedMethods := 
		self methods inject: Set new into: 
							[:sums :cm | sums union: cm messages].
	testedMethods := testedMethods reject: [:sel | (definedMethods includes: sel) not].
	untestedMethods := definedMethods select: [:selector | (testedMethods includes: selector) not].
	^ { (testedMethods size * 100 / definedMethods size) asFloat . untestedMethods}
