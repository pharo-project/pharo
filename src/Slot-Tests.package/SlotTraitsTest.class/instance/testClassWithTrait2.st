tests
testClassWithTrait2

	self assert: TOne traitUsers isEmpty.
	self assert: TOne classTrait traitUsers isEmpty.
	
	aClass := self make: [ :builder | 
		builder traitComposition: TOne classTrait ].
	
	self assert: TOne traitUsers isEmpty.
	self assert: TOne classTrait traitUsers asArray equals: { aClass }.
	
	self deny: (aClass canUnderstand: #one).
	self assert: (aClass canUnderstand: #classOne).
	self assert: aClass new classOne equals: 1.