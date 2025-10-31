Class {
	#name : 'ReMethodsDontReferToInstVarsCondition',
	#superclass : 'ReMethodsCondition',
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'initialization' }
ReMethodsDontReferToInstVarsCondition >> class: aRBClass selectors: aCollection [ 

	class := aRBClass.
	selectors := aCollection 
]

{ #category : 'accessing' }
ReMethodsDontReferToInstVarsCondition >> nonViolators [
	"I override nonViolators because my violators differ in type from my candidates.
	My violators are pairs of the form {selector, instVarName}, while my candidates are selectors."
	^ (
		 (self candidates difference: (self violators collect: [ :assoc | assoc first ])) 
			collect: [ :nonViolator | { nonViolator. nil } ]
		) asSet
]

{ #category : 'accessing' }
ReMethodsDontReferToInstVarsCondition >> referencedInstanceVariables [

	^ self violators collect: [ :violator | violator at: 2 ]
]

{ #category : 'displaying' }
ReMethodsDontReferToInstVarsCondition >> violationMessageOn: aStream [ 
	self violators do: [ :violator |
		| selector instVar |
		selector := violator first.
		instVar := violator second.
		aStream
			nextPutAll: 'Method ''';
			nextPutAll: selector;
			nextPutAll: ''' refers to the inst var #';
			nextPutAll: instVar;
			nextPutAll: ' defined in class ';
			nextPutAll: class name ]
]

{ #category : 'displaying' }
ReMethodsDontReferToInstVarsCondition >> violationMessageOn: aStream violators: theViolators [

	theViolators ifEmpty: [ ^ self ].

	theViolators do: [ :violator |
			| selector instVar |
			selector := violator first.
			instVar := violator second.
			aStream
				nextPutAll: 'Method ''';
				nextPutAll: selector;
				nextPutAll: ''''.
			theViolators = self violators
				ifTrue: [
						aStream
							nextPutAll: ' refers to the inst var #';
							nextPutAll: instVar;
							nextPutAll: ' defined in class ';
							nextPutAll: class name ]
				ifFalse: [ aStream nextPutAll: ' does not refer any instVar' ] ]
]

{ #category : 'accessing' }
ReMethodsDontReferToInstVarsCondition >> violators [

	violators ifNotNil: [ ^ violators ].
	
	violators := Set new.
	self candidates do: [ :selector |
		class instanceVariableNames do: [ :instVar |
			((class methodFor: selector) refersToVariable: instVar)
				ifTrue: [ violators add: { selector . instVar } ] ] ].
	^ violators asOrderedCollection
]
