Class {
	#name : 'ReUnoptimizedToDoRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Optimization',
	#package : 'General-Rules-Tests',
	#tag : 'Optimization'
}

{ #category : 'tests' }
ReUnoptimizedToDoRuleTest >> testRule [

	self generateMethod: 'method (1 to: 10) do: [ :each | each ]'.
	self assert: (self myCritiquesOnMethod: self class >> #method) size equals: 1
]

{ #category : 'tests' }
ReUnoptimizedToDoRuleTest >> testRuleNotViolated [

	self generateMethod: 'method 1 to: 10 do: [ :each | each ] '.
	self assertEmpty: (self myCritiquesOnMethod: self class >> #method)
]
