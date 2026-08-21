"
A RBMoveMethodToClassRefactoring is a class that represents functionality of ""Move method to class"" refactoring.
User chooses method, and than any of existiong classes.
Refactoring moves chosen method to class.

Instance Variables
	method:		<RBMethod>

method
	- chosen method

"
Class {
	#name : 'ReMoveMethodToClassRefactoring',
	#superclass : 'RBMethodRefactoring',
	#instVars : [
		'method'
	],
	#category : 'Refactoring-Transformations',
	#package : 'Refactoring-Transformations'
}

{ #category : 'instance creation' }
ReMoveMethodToClassRefactoring class >> method: aMethod class: aClass [
	^ self new
		method: aMethod class: aClass;
		yourself
]

{ #category : 'instance creation' }
ReMoveMethodToClassRefactoring class >> model: aRBSmalltalk method: aMethod class: aClass [
	^ self new
		model: aRBSmalltalk;
		method: aMethod class: aClass;
		yourself
]

{ #category : 'preconditions' }
ReMoveMethodToClassRefactoring >> applicabilityPreconditions [
	"we only move if there is no method in the class. 
	This is clearly wrong for a refactoring. but for now let us keep it. "
	^ { (ReDefinesSelectorsCondition new definesSelectors: {method selector} in: class) not }
]

{ #category : 'transforming' }
ReMoveMethodToClassRefactoring >> classModelOf: aClass [
	^ self model classObjectFor: aClass
]

{ #category : 'accessing' }
ReMoveMethodToClassRefactoring >> method [ 

	^ method
]

{ #category : 'initialization' }
ReMoveMethodToClassRefactoring >> method: aMethod class: aClassName [

	method := aMethod.
	class := self classObjectFor: aClassName
]

{ #category : 'transforming' }
ReMoveMethodToClassRefactoring >> privateTransform [
	| oldClass newClass rbMethod originalProtocol |
	oldClass := self classModelOf: method methodClass.
	newClass := self classModelOf: class.
	rbMethod := RBMethod for: newClass source: method sourceCode selector: method selector.
	originalProtocol := method protocolName.
	self generateChangesFor:
		(RBAddMethodTransformation
			sourceCode: rbMethod source
			in: newClass
			withProtocol: originalProtocol).
	self generateChangesFor: 
		(RBRemoveMethodTransformation 
			selector: method selector
			from: oldClass)
]
