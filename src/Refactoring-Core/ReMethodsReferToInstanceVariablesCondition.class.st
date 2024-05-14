Class {
	#name : 'ReMethodsReferToInstanceVariablesCondition',
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
ReMethodsReferToInstanceVariablesCondition >> class: aRBClass selectors: aCollection [ 

	class := aRBClass.
	selectors := aCollection 
]

{ #category : 'accessing' }
ReMethodsReferToInstanceVariablesCondition >> violators [

	violators ifNotNil: [ ^ violators ].

	violators := Set new.
	selectors do: [ :selector |
		class instanceVariableNames do: [ :instVar |
			((class methodNamed: selector) refersToVariable: instVar)
				ifFalse: [ violators add: selector ] ] ].
	^ violators asOrderedCollection
]
