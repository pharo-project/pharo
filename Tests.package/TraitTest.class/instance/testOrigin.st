testing
testOrigin
	| tr1 tr2 tr3 tr23 aMethodDescription |
	tr1 := self createTraitNamed: #TTT1 uses: {}.
	tr2 := self createTraitNamed: #TTT2 uses: {tr1}.
	tr3 := self createTraitNamed: #TTT3 uses: {tr1}.
	tr23 := self createTraitNamed: #TTT23 uses: {tr3 + tr2}.
	
	tr1 compile: 'foo ^ 4'.
	self assert: (tr1 >> #foo) origin == tr1.
	self assert: (tr2 >> #foo) origin == tr1.
	self assert: (tr3 >> #foo) origin == tr1.
	
	"-----------"
	"For TR2"
	aMethodDescription := tr2 traitComposition methodDescriptionForSelector: #foo.
	self assert: (aMethodDescription locatedMethods size = 1).
	self assert: (aMethodDescription locatedMethods includes: ( tr1 >>  #foo)).

	self assert: (aMethodDescription providedLocatedMethod notNil).	
	"The method is provided, it cannot be nil"
	self assert: (aMethodDescription providedMethod notNil).	
	self assert: (aMethodDescription isProvided).	
		
	self assert: (tr2 traitComposition traitProvidingSelector: #foo) == tr1.
	self assert: (tr2 >> #foo) origin == tr1.
	"-----------"
	
	"-----------"
	"For TR23"
	aMethodDescription := tr23 traitComposition methodDescriptionForSelector: #foo.
	self assert: (aMethodDescription locatedMethods size = 2).
	self assert: (aMethodDescription locatedMethods includes: (tr2 >> #foo)).
	self assert: (aMethodDescription locatedMethods includes: (tr3 >> #foo)).

	self assert: (aMethodDescription providedLocatedMethod notNil).	
	"The method is provided, it cannot be nil"
	self assert: (aMethodDescription providedMethod notNil).	

	self assert: (aMethodDescription isProvided).	
		
	self assert: (tr23 traitComposition traitProvidingSelector: #foo) == tr1.
	self assert: (tr23 >> #foo) origin == tr1.
	"----------"