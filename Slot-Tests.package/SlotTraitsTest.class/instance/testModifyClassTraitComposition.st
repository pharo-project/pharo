tests
testModifyClassTraitComposition
	"Tests that when modifing the trait composition on class-side, the old methods are removed from the method dictionary, and the new ones are added."

	self make: [ :builder | 
		builder classTraitComposition: TOne ].

	aClass := self make: [ :builder | 
		builder classTraitComposition: TTwo ].
	
	self assert: TOne traitUsers isEmpty.
	self assert: TOne classTrait traitUsers isEmpty.
	self assert: TTwo traitUsers asArray equals: { aClass class }.
	self assert: TTwo classTrait traitUsers isEmpty.
	
	self deny: (aClass class canUnderstand: #one).
	self deny: (aClass class canUnderstand: #classOne).
	self assert: (aClass class canUnderstand: #two).
	aClass classLayout checkIntegrity.


	aClass := self make: [ :builder | 
		builder classTraitComposition: { } ].
	
	self assert: TOne traitUsers isEmpty.
	self assert: TOne classTrait traitUsers isEmpty.
	self assert: TTwo traitUsers isEmpty.
	self assert: TTwo classTrait traitUsers isEmpty.
	aClass classLayout checkIntegrity.
	
	self deny: (aClass canUnderstand: #one).
	self deny: (aClass class canUnderstand: #classOne).
	self deny: (aClass canUnderstand: #two).