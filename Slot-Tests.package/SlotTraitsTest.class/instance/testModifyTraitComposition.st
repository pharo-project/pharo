tests
testModifyTraitComposition
	"Tests that when modifing the trait composition, the old methods are removed from the method dictionary, and the new ones are added."
	
	self make: [ :builder | 
		builder traitComposition: TOne ].

	aClass := self make: [ :builder | 
		builder traitComposition: TTwo ].
	
	self assert: TOne traitUsers isEmpty.
	self assert: TOne classTrait traitUsers isEmpty.
	self assert: TTwo traitUsers asArray equals: { aClass }.
	"self assert: TTwo classTrait traitUsers isEmpty."
	aClass classLayout checkIntegrity.
	
	self deny: (aClass canUnderstand: #one).
	self deny: (aClass class canUnderstand: #classOne).
	self assert: (aClass canUnderstand: #two).
	
	
	aClass := self make: [ :builder | 
		builder traitComposition: { } ].
	
	self assert: TOne traitUsers isEmpty.
	self assert: TOne classTrait traitUsers isEmpty.
	self assert: TTwo traitUsers isEmpty.
	self assert: TTwo classTrait traitUsers isEmpty.
	aClass classLayout checkIntegrity.
	
	self deny: (aClass canUnderstand: #one).
	self deny: (aClass class canUnderstand: #classOne).
	self deny: (aClass canUnderstand: #two).
	
