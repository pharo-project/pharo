"
Adds a comment in the beginning of the method (after its signature).

Usage:
| transformation |
transformation := (RBAddMethodCommentTransformation
		comment: 'New comment'
		inMethod: #testTransform
		inClass: #RBAddClassTransformationTest)
		transform.
(ChangesBrowser changes: transformation model changes changes) open
"
Class {
	#name : #RBAddMethodCommentTransformation,
	#superclass : #RBMethodTransformation,
	#instVars : [
		'comment'
	],
	#category : #'Refactoring2-Transformations-Model'
}

{ #category : #api }
RBAddMethodCommentTransformation class >> comment: aString inMethod: aSelector inClass: aClassName [

	^ (self new)
		comment: aString 
		inMethod: aSelector
		inClass: aClassName;
		yourself
]

{ #category : #api }
RBAddMethodCommentTransformation class >> model: aRBModel comment: aString inMethod: aSelector inClass: aClassName [

	^ (self new)
		model: aRBModel;
		comment: aString 
		inMethod: aSelector
		inClass: aClassName;
		yourself
]

{ #category : #api }
RBAddMethodCommentTransformation >> comment: aString inMethod: aSelector inClass: aClassName [

	self className: aClassName.
	selector := aSelector.
	comment := aString
	
]

{ #category : #preconditions }
RBAddMethodCommentTransformation >> preconditions [
	"no preconditions needed"
	
	^ RBCondition empty
]

{ #category : #executing }
RBAddMethodCommentTransformation >> privateTransform [
	| tree signature sourceCode |
	tree := self definingMethod.
	signature := tree source copyFrom: 1 to: tree body start - 1.
	sourceCode := signature , String cr , (comment surroundedBy: '"')
		, String cr , tree body sourceCode.
	self definingClass
		compileTree: (self parserClass parseMethod: sourceCode)
]

{ #category : #printing }
RBAddMethodCommentTransformation >> storeOn: aStream [

	aStream nextPut: $(.
	self class storeOn: aStream.
	aStream
		nextPutAll: ' comment: ''';
		nextPutAll: comment;
		nextPutAll: ''' inMethod: ''';
		nextPutAll: selector;
		nextPutAll: ''' inClass: '.
	class storeOn: aStream.
	aStream nextPut: $)
]
