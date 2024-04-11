"
I am a refactoring condition that checks if a method refers to any of the shared variables directly defined in its class. 
I do not check for shared variables defined in my class superclasses
"
Class {
	#name : 'ReMethodsReferToSharedVariablesCondition',
	#superclass : 'ReMethodsCondition',
	#instVars : [
		'class',
		'selectors'
	],
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'initialization' }
ReMethodsReferToSharedVariablesCondition >> class: aRBClass selectors: aCollection [ 

	class := aRBClass.
	selectors := aCollection 
]

{ #category : 'accessing' }
ReMethodsReferToSharedVariablesCondition >> violators [

	violators ifNotNil: [ ^ violators ].

	violators := Set new.
	selectors do: [ :selector |
		class classVariableNames do: [ :classVar |
			((class methodNamed: selector) refersToVariable: classVar)
				ifFalse: [ violators add: selector ] ] ].
	^ violators asOrderedCollection
]
