Class {
	#name : 'ReLongMethodsRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Design Flaws',
	#package : 'General-Rules-Tests',
	#tag : 'Design Flaws'
}

{ #category : 'lint' }
ReLongMethodsRuleTest >> longMethods [

	| n |
	n := 1.
	n := 2.
	n := 3.
	n := 4.
	n := 5.
	n := 6.
	n := 7.
	n := 8.
	n := 9.
	n := 10.
	n := 11.
	n := 12.
	n := 13.
	n := 14.
	n := 15.
	n := 16.
	n := 16.
	n := 17.
	n := 18.
	n := 19
]

{ #category : 'lint' }
ReLongMethodsRuleTest >> smallMethod [

	| n |
	n := 1.
	n := 2.
	n := 3.
	n := 4.
	n := 5.
	n := 6.
	n := 7.
	n := 8.
	n := 9.
	n := 10.
	n := 11.
	n := 12.
	n := 13.
	n := 14.
	n := 15.
	n := 16.
	n := 16.
	n := 17.
	n := 18
]

{ #category : 'tests' }
ReLongMethodsRuleTest >> testRule [
	|critiques| 
	critiques := self myCritiquesOnMethod: self class >> #longMethods.
	self assert: critiques size equals: 1 

	
]

{ #category : 'tests' }
ReLongMethodsRuleTest >> testRuleNotViolated [
	|critiques| 
	critiques := self myCritiquesOnMethod: self class >> #smallMethod.
	self assertEmpty: critiques

	
]
