Class {
	#name : #ReRuleManagerTest,
	#superclass : #RenrakuBaseTestCase,
	#category : #'Renraku-Tests'
}

{ #category : #tests }
ReRuleManagerTest >> testClassAdded [

	| rule rulesCount |
	
	ReRuleManager reset.
	
	rulesCount := ReRuleManager default nodeRules size.

	rule := self class classInstaller make: [ :aClassBuilder | 
		aClassBuilder 
			name: #ReRuleManagerTestingRule;
			superclass: ReNodeMatchRule;
			package: testPackage name ].
			
	rule compile: 'name ^ ''test'''.

	self
		assert: ReRuleManager default nodeRules size - rulesCount
		equals: 1.

	self assert:
		(ReRuleManager default allRules anySatisfy: [ :r |
			r class == rule ]).
		
	rule removeFromSystem.
]

{ #category : #tests }
ReRuleManagerTest >> testClassRemoved [

	| rule |
	
	rule := self class classInstaller make: [ :aClassBuilder | 
		aClassBuilder 
			name: #ReRuleManagerTestingRule;
			superclass: ReAbstractRule;
			package: testPackage name ].

	rule class
		compile: 'checksMethod ^ true'.
		
	rule compile: 'name ^ ''test'''.

	ReRuleManager reset.

	self assert:
		(ReRuleManager default allRules anySatisfy: [ :r |
			r class == rule ]).
		
	rule removeFromSystem.
		
	self assert:
		(ReRuleManager default allRules noneSatisfy: [ :r |
			r class == rule ]).
]
