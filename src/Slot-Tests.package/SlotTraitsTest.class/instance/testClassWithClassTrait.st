tests
testClassWithClassTrait

	self assert: TOne traitUsers isEmpty.
	self assert: TOne classTrait traitUsers isEmpty.
	
	aClass := self make: [ :builder | 
		builder classTraitComposition: TOne ].
	
	self assert: TOne traitUsers asArray equals: { aClass class }.
	self assert: TOne classTrait traitUsers isEmpty.
	
	self assert: (aClass class canUnderstand: #one).
	self assert: aClass one equals: 1.