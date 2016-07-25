tests
testClassWithTrait

	self assert: TOne traitUsers isEmpty.
	self assert: TOne classTrait traitUsers isEmpty.
	
	aClass := self make: [ :builder | 
		builder traitComposition: TOne ].
	
	self assert: TOne traitUsers asArray equals: { aClass }.
	self assert: TOne classTrait traitUsers isEmpty.
	
	self assert: (aClass canUnderstand: #one).
	self assert: aClass new one equals: 1.