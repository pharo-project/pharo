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
	#name : #RBMoveMethodToClassRefactoring,
	#superclass : #RBMethodRefactoring,
	#instVars : [
		'method'
	],
	#category : #'Refactoring-Core-Refactorings'
}

{ #category : #'instance creation' }
RBMoveMethodToClassRefactoring class >> method: aMethod class: aClass [
	^ self new
		method: aMethod class: aClass;
		yourself
]

{ #category : #'instance creation' }
RBMoveMethodToClassRefactoring class >> model: aRBSmalltalk method: aMethod class: aClass [
	^ self new
		model: aRBSmalltalk;
		method: aMethod class: aClass;
		yourself
]

{ #category : #transforming }
RBMoveMethodToClassRefactoring >> classModelOf: aClass [
	| classModel |
	classModel := aClass isMeta 
		ifTrue: [ RBClassModelFactory rbMetaclass named: aClass name ]
		ifFalse: [ RBClassModelFactory rbClass named: aClass name ].
	classModel model: self model.
	^classModel
]

{ #category : #initialization }
RBMoveMethodToClassRefactoring >> method: aMethod class: aClass [
	method := aMethod.
	class := self classObjectFor: aClass.
]

{ #category : #preconditions }
RBMoveMethodToClassRefactoring >> preconditions [
	^(RBCondition definesSelector: method selector in: class) not
]

{ #category : #printing }
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

{ #category : #transforming }
RBMoveMethodToClassRefactoring >> transform [
	| oldClass newClass rbMethod originalProtocol |
	oldClass := self classModelOf: method methodClass.
	newClass := self classModelOf: class.
	rbMethod := RBClassModelFactory rbMethod for: newClass source: method sourceCode selector: method selector.
	originalProtocol := method protocol.
	oldClass removeMethod: method selector.
	newClass addMethod: rbMethod.
	newClass compile: rbMethod source classified: {originalProtocol}
]
