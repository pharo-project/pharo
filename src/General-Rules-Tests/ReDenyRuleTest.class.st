Class {
	#name : 'ReDenyRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Migrated',
	#package : 'General-Rules-Tests',
	#tag : 'Migrated'
}

{ #category : 'actions' }
ReDenyRuleTest >> testRule [

	|critiques| 
	self class compile: 'method self assert: true equals: false' classified: 'test-help'.
	[ critiques := self myCritiquesOnMethod: self class >>#method.
	self assert: critiques size equals: 1 ] ensure: [ (self class >> #method) removeFromSystem ].
]

{ #category : 'actions' }
ReDenyRuleTest >> testRuleNotViolated [

	|critiques| 
	self class compile: 'method self assert: true equals: true' classified: 'test-help'.
	[ critiques := self myCritiquesOnMethod: self class >>#method.
	self assertEmpty: critiques  ] ensure: [ (self class >> #method) removeFromSystem ]
]
