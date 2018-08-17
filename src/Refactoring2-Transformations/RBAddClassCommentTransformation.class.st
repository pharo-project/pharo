"
Adds a comment to a class. It replaces the current comment for the new one (i.e., it does not append the given text to the existing class comment)

Usage:
| transformation |
transformation := (RBAddClassCommentTransformation
		comment: 'New comment'
		in: RBTransformationTest)
		transform.
(ChangesBrowser changes: transformation model changes changes) open
"
Class {
	#name : #RBAddClassCommentTransformation,
	#superclass : #RBClassTransformation,
	#instVars : [
		'comment'
	],
	#category : #'Refactoring2-Transformations-Model'
}

{ #category : #api }
RBAddClassCommentTransformation class >> comment: aString in: aClass [

	^ (self new)
		comment: aString
		in: aClass;
		yourself
]

{ #category : #api }
RBAddClassCommentTransformation class >> model: aRBModel comment: aString in: aClass [

	^ (self new)
		model: aRBModel;
		comment: aString
		in: aClass;
		yourself
]

{ #category : #api }
RBAddClassCommentTransformation >> comment: aString in: aClassName [ 

	self className: aClassName.
	comment := aString
]

{ #category : #preconditions }
RBAddClassCommentTransformation >> preconditions [
	"no preconditions needed"
	
	^ RBCondition empty
]

{ #category : #executing }
RBAddClassCommentTransformation >> privateTransform [

	self definingClass comment: comment
]

{ #category : #printing }
RBAddClassCommentTransformation >> storeOn: aStream [ 

	aStream nextPut: $(.
	self class storeOn: aStream.
	aStream
		nextPutAll: ' comment: ''';
		nextPutAll: comment;
		nextPutAll: ''' in: ''';
		nextPutAll: className.
	aStream nextPut: $)
]
