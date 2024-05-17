Class {
	#name : 'ReAndOrRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Migrated',
	#package : 'General-Rules-Tests',
	#tag : 'Migrated'
}

{ #category : 'tests' }
ReAndOrRuleTest >> testRule [ 

	|critiques |
	self class compile: 'method false & true. true | true' classified: 'test-help'.
	[ critiques := self myCritiquesOnMethod: self class >> #method. 
	self assert: critiques size equals: 2 ] ensure: [ (self class >> #method) removeFromSystem ]

]

{ #category : 'tests' }
ReAndOrRuleTest >> testRuleNotViolated [

	|critiques |
	self class compile: 'method false and: [ true ]. true or: [ true ]' classified: 'test-help'.
	[ critiques := self myCritiquesOnMethod: self class >> #method. 
	self assertEmpty: critiques  ] ensure: [ (self class >> #method) removeFromSystem ]

]
