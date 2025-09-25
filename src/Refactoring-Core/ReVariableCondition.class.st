Class {
	#name : 'ReVariableCondition',
	#superclass : 'RBNewAbstractCondition',
	#instVars : [
		'aClass',
		'violators',
		'variable'
	],
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'accessing' }
ReVariableCondition >> nonViolators [

	^ self violatorCandidates reject: [ :each | self violators anySatisfy: [ :v | v == each ] ]
]

{ #category : 'accessing' }
ReVariableCondition >> violatorCandidates [

	self subclassResponsibility
]

{ #category : 'accessing' }
ReVariableCondition >> violators [
	self subclassResponsibility 
]
