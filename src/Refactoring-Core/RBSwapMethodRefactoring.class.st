"
Move a method from the class to the instance side, or vice versa. Normally this is not considered to be a refactoring.

Only instance methods with no instance variable access or class methods with no class instance variable access can be moved.
"
Class {
	#name : #RBSwapMethodRefactoring,
	#superclass : #RBMethodRefactoring,
	#instVars : [
		'target',
		'selector'
	],
	#category : #'Refactoring-Core-Refactorings'
}

{ #category : #'instance-creation' }
RBSwapMethodRefactoring class >> model: aRBSmalltalk swapMethod: aSelector in: aClass [
	^ self new 
		model: aRBSmalltalk;
		swapMethod: aSelector in: aClass;
		yourself
]

{ #category : #'instance-creation' }
RBSwapMethodRefactoring class >> swapMethod: aSelector in: aClass [
	^ self new swapMethod: aSelector in: aClass
]

{ #category : #preconditions }
RBSwapMethodRefactoring >> checkInstVars [
	class instanceVariableNames do: [ :each |
		(target instanceVariableNames includes: each) ifFalse: [
			((class whichSelectorsReferToInstanceVariable: each) includes: selector) ifTrue: [
				self
					refactoringError: ('<1p> refers to <2s>, which is not defined in <3p>' expandMacrosWith: selector with: each with: target) ] ] ]
]

{ #category : #preconditions }
RBSwapMethodRefactoring >> preconditions [
	^ (RBCondition definesSelector: selector in: class)
		& (RBCondition definesSelector: selector in: target) not
		& (RBCondition withBlock: [ self checkInstVars. true ])
]

{ #category : #initialization }
RBSwapMethodRefactoring >> swapMethod: aSelector in: aClass [ 
	class := self classObjectFor: aClass.
	target := self classObjectFor: (class isMeta 
			ifTrue: [ class theNonMetaClass ]
			ifFalse: [ class theMetaClass ]).
	selector := aSelector
]

{ #category : #transforming }
RBSwapMethodRefactoring >> transform [
	target compile: (class sourceCodeFor: selector) classified: (class protocolsFor: selector).
	class removeMethod: selector
]
