Class {
	#name : 'ReMethodsDontReferToInstVarsCondition',
	#superclass : 'ReMethodsCondition',
	#instVars : [
		'class',
		'selectors'
	],
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'choices' }
ReMethodsDontReferToInstVarsCondition class >> strategyChoiceClass [
	^ RePushUpInstanceVariableChoice
]

{ #category : 'initialization' }
ReMethodsDontReferToInstVarsCondition >> class: aRBClass selectors: aCollection [ 

	class := aRBClass.
	selectors := aCollection 
]

{ #category : 'accessing' }
ReMethodsDontReferToInstVarsCondition >> referencedInstanceVariables [

	^ self violators collect: [ :violator | violator at: 2 ]
]

{ #category : 'displaying' }
ReMethodsDontReferToInstVarsCondition >> violationMessageOn: aStream [

	self violators do: [ :violator |
		| selector instVar |
		selector := violator at: 1.
		instVar := violator at: 2.
		aStream
			nextPutAll: '''';
			nextPutAll: selector;
			nextPutAll: ''' refers to the inst var #';
			nextPutAll: instVar ]
]

{ #category : 'accessing' }
ReMethodsDontReferToInstVarsCondition >> violators [

	violators ifNotNil: [ ^ violators ].
	
	violators := Set new.
	selectors do: [ :selector |
		class instanceVariableNames do: [ :instVar |
			((class methodFor: selector) refersToVariable: instVar)
				ifTrue: [ violators add: { selector . instVar } ] ] ].
	^ violators asOrderedCollection
]
