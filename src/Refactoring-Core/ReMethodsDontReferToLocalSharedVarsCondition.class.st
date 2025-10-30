"
I am a refactoring condition that checks if a method refers to any of the shared variables directly defined in its class. 
I do not check for shared variables defined in my class superclasses
"
Class {
	#name : 'ReMethodsDontReferToLocalSharedVarsCondition',
	#superclass : 'ReMethodsCondition',
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'initialization' }
ReMethodsDontReferToLocalSharedVarsCondition >> class: aRBClass selectors: aCollection [ 

	class := aRBClass.
	selectors := aCollection 
]

{ #category : 'accessing' }
ReMethodsDontReferToLocalSharedVarsCondition >> nonViolators [
	"I override nonViolators because my violators differ in type from my candidates.
	My violators are pairs of the form {selector, instVarName}, while my candidates are selectors."
	^ (
		 (self candidates difference: (self violators collect: [ :assoc | assoc first ])) 
			collect: [ :nonViolator | { nonViolator. nil } ]
		) asSet
]

{ #category : 'accessing' }
ReMethodsDontReferToLocalSharedVarsCondition >> referencedSharedVariables [

	^ self violators collect: [ :violator | violator at: 2 ]
]

{ #category : 'displaying' }
ReMethodsDontReferToLocalSharedVarsCondition >> violationMessageOn: aStream [
	self deprecated: 'Use violationMessage:violators: instead'.
	self violators do: [ :violator |
		| selector instVar |
		selector := violator first.
		instVar := violator second.
		aStream
			nextPutAll: '''';
			nextPutAll: selector;
			nextPutAll: ''' refers to the shared var #';
			nextPutAll: instVar ]
]

{ #category : 'displaying' }
ReMethodsDontReferToLocalSharedVarsCondition >> violationMessageOn: aStream violators: theViolators [
	| verb |
	theViolators ifEmpty: [ ^self ].
	verb := self violators = theViolators ifTrue: [ ' refers' ] ifFalse: [ ' does not refer' ].
	
	theViolators do: [ :violator |
		| selector instVar |
		selector := violator first.
		instVar := violator second.
		aStream
			nextPutAll: '''';
			nextPutAll: selector;
			nextPutAll: '''';
			nextPutAll: verb;
			nextPutAll: ' to the shared var #';
			nextPutAll: instVar ]
]

{ #category : 'accessing' }
ReMethodsDontReferToLocalSharedVarsCondition >> violators [

	violators ifNotNil: [ ^ violators ].

	violators := Set new.
	self candidates do: [ :selector |
		class classVariableNames do: [ :classVar |
			((class methodFor: selector) refersToVariable: classVar)
				ifTrue: [ violators add: { selector . classVar } ] ] ].
	^ violators asOrderedCollection
]
