tests
testRemoveClassTrait
	"Tests that when removing a trait from class-side, its methods are removed from the method dictionary."

	aClass := self make: [ :builder | 
		builder classTraitComposition: TOne ].

	aClass := self make: [ :builder | 
		builder classTraitComposition: {} ].
	
	self deny: (aClass class canUnderstand: #one).
	self assert: TOne traitUsers isEmpty.
	self assert: TOne classTrait traitUsers isEmpty.
