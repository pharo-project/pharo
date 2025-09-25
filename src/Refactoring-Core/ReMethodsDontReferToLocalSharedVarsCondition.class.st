"
I am a refactoring condition that checks if a method refers to any of the shared variables directly defined in its class. 
I do not check for shared variables defined in my class superclasses
"
Class {
	#name : 'ReMethodsDontReferToLocalSharedVarsCondition',
	#superclass : 'ReMethodsCondition',
	#instVars : [
		'class',
		'selectors',
		'variables'
	],
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'initialization' }
ReMethodsDontReferToLocalSharedVarsCondition >> class: aRBClass selectors: aCollection [ 

	class := aRBClass.
	selectors := aCollection.
	variables := nil
]

{ #category : 'initialization' }
ReMethodsDontReferToLocalSharedVarsCondition >> class: aRBClass selectors: aCollection variables: aCollection2 [

	class := aRBClass.
	selectors := aCollection.
	variables := aCollection2 
]

{ #category : 'accessing' }
ReMethodsDontReferToLocalSharedVarsCondition >> referencedSharedVariables [

	^ self violators collect: [ :violator | violator at: 2 ]
]

{ #category : 'accessing' }
ReMethodsDontReferToLocalSharedVarsCondition >> variables [
	^ variables ifNil: [ variables := class classVariableNames ]
]

{ #category : 'displaying' }
ReMethodsDontReferToLocalSharedVarsCondition >> violationMessageOn: aStream [

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
ReMethodsDontReferToLocalSharedVarsCondition >> violators [

	violators ifNotNil: [ ^ violators ].

	violators := Set new.
	selectors do: [ :selector |
		self variables do: [ :classVar |
			((class methodFor: selector) refersToVariable: classVar)
				ifTrue: [ violators add: { selector . classVar } ] ] ].
	^ violators asOrderedCollection
]
