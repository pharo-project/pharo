"
I am a refactoring condition that checks if a method refers to any of the shared variables directly defined in its class. 
I do not check for shared variables defined in my class superclasses
"
Class {
	#name : 'ReMethodsDontReferToSharedVarsCondition',
	#superclass : 'ReMethodsCondition',
	#instVars : [
		'class',
		'selectors'
	],
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'as yet unclassified' }
ReMethodsDontReferToSharedVarsCondition class >> strategyChoiceClass [
	^ RePushUpSharedVariableChoice
]

{ #category : 'initialization' }
ReMethodsDontReferToSharedVarsCondition >> class: aRBClass selectors: aCollection [ 

	class := aRBClass.
	selectors := aCollection 
]

{ #category : 'displaying' }
ReMethodsDontReferToSharedVarsCondition >> violationMessageOn: aStream [

	self violators do: [ :violator |
		| selector instVar |
		selector := violator at: 1.
		instVar := violator at: 2.
		aStream
			nextPutAll: '''';
			nextPutAll: selector;
			nextPutAll: ''' refers to the shared var #';
			nextPutAll: instVar ]
]

{ #category : 'accessing' }
ReMethodsDontReferToSharedVarsCondition >> violators [

	violators ifNotNil: [ ^ violators ].

	violators := Set new.
	selectors do: [ :selector |
		class classVariableNames do: [ :classVar |
			((class methodFor: selector) refersToVariable: classVar)
				ifTrue: [ violators add: { selector . classVar } ] ] ].
	^ violators asOrderedCollection
]
