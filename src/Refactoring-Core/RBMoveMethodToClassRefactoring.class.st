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
	#name : 'RBMoveMethodToClassRefactoring',
	#superclass : 'RBMethodRefactoring',
	#instVars : [
		'method'
	],
	#category : 'Refactoring-Core-Refactorings',
	#package : 'Refactoring-Core',
	#tag : 'Refactorings'
}

{ #category : 'instance creation' }
RBMoveMethodToClassRefactoring class >> method: aMethod class: aClass [
	^ self new
		method: aMethod class: aClass;
		yourself
]

{ #category : 'instance creation' }
RBMoveMethodToClassRefactoring class >> model: aRBSmalltalk method: aMethod class: aClass [
	^ self new
		model: aRBSmalltalk;
		method: aMethod class: aClass;
		yourself
]

{ #category : 'preconditions' }
RBMoveMethodToClassRefactoring >> applicabilityPreconditions [
	"we only move if there is no method in the class. 
	This is clearly wrong for a refactoring. but for now let us keep it. "
	^ { (ReDefinesSelectorsCondition new definesSelectors: {method selector} in: class) not }
]

{ #category : 'transforming' }
RBMoveMethodToClassRefactoring >> classModelOf: aClass [
	^ self model classObjectFor: aClass
]

{ #category : 'accessing' }
RBMoveMethodToClassRefactoring >> method [ 

	^ method
]

{ #category : 'initialization' }
RBMoveMethodToClassRefactoring >> method: aMethod class: aClassName [

	method := aMethod.
	class := self classObjectFor: aClassName
]

{ #category : 'transforming' }
RBMoveMethodToClassRefactoring >> privateTransform [
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

{ #category : 'storing' }
RBMoveMethodToClassRefactoring >> storeOn: aStream [
	aStream nextPut: $(.
	self class storeOn: aStream.
	aStream
		nextPutAll: ' selector: #';
		nextPutAll:  method selector;
		nextPutAll: ' class: '.
	class storeOn: aStream.
	aStream
		nextPutAll: ''')'
]
