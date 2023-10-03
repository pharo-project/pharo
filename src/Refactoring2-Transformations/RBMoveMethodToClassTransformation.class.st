"
I am responsible for moving a method from one class to another existing class

Usage:
| transformation |
transformation := (RBMoveMethodToClassTransformation
				method: #method
				class: #RBMoveMethodTransformationTest)
				transform.
(ChangesBrowser changes: transformation model changes changes) open

"
Class {
	#name : 'RBMoveMethodToClassTransformation',
	#superclass : 'RBMethodTransformation',
	#category : 'Refactoring2-Transformations-Model-Unused',
	#package : 'Refactoring2-Transformations',
	#tag : 'Model-Unused'
}

{ #category : 'initialization' }
RBMoveMethodToClassTransformation class >> method: aMethod class: aClass [
	^ self new
		method: aMethod class: aClass;
		yourself
]

{ #category : 'initialization' }
RBMoveMethodToClassTransformation class >> model: aRBSmalltalk method: aMethod class: aClass [
	^ self new
		model: aRBSmalltalk;
		method: aMethod class: aClass;
		yourself
]

{ #category : 'transforming' }
RBMoveMethodToClassTransformation >> applicabilityPreconditions [

	^ (RBCondition definesSelector: method selector in: class) not
]

{ #category : 'transforming' }
RBMoveMethodToClassTransformation >> classModelOf: aClass [
	| classModel |
	classModel := aClass isMeta
		ifTrue: [ RBClassModelFactory rbMetaclass named: aClass name ]
		ifFalse: [ RBClassModelFactory rbClass named: aClass name ].
	classModel model: self model.
	^classModel
]

{ #category : 'transforming' }
RBMoveMethodToClassTransformation >> method: aMethod class: aClass [
	method := aMethod.
	class := self model classObjectFor: aClass
]

{ #category : 'transforming' }
RBMoveMethodToClassTransformation >> privateTransform [
	| oldClass newClass rbMethod originalProtocol |
	oldClass := self classModelOf: method methodClass.
	newClass := self classModelOf: class.
	rbMethod := RBClassModelFactory rbMethod for: newClass source: method sourceCode selector: method selector.
	originalProtocol := method protocolName.
	oldClass removeMethod: method selector.
	newClass addMethod: rbMethod.
	(RBAddMethodTransformation
		model: self model
		sourceCode: rbMethod
		in: newClass
		withProtocol: originalProtocol) generateChanges
]

{ #category : 'printing' }
RBMoveMethodToClassTransformation >> storeOn: aStream [
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
