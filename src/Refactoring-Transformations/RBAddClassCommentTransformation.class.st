"
Adds a comment to a class. It replaces the current comment for the new one (i.e., it does not append the given text to the existing class comment)

Usage:
```
| transformation |
transformation := (RBAddClassCommentTransformation
		comment: 'New comment'
		in: RBClassToRename)
		generateChanges.
(StRefactoringPreviewPresenter for: transformation) open
```
"
Class {
	#name : 'RBAddClassCommentTransformation',
	#superclass : 'RBClassTransformation',
	#instVars : [
		'comment'
	],
	#category : 'Refactoring-Transformations-Model-Unused',
	#package : 'Refactoring-Transformations',
	#tag : 'Model-Unused'
}

{ #category : 'api' }
RBAddClassCommentTransformation class >> comment: aString in: aClass [

	^ self new
		comment: aString
		in: aClass;
		yourself
]

{ #category : 'api' }
RBAddClassCommentTransformation class >> model: aRBModel comment: aString in: aClass [

	^ self new
		model: aRBModel;
		comment: aString
		in: aClass;
		yourself
]

{ #category : 'api' }
RBAddClassCommentTransformation >> comment: aString in: aClassName [

	self className: aClassName.
	comment := aString
]

{ #category : 'executing' }
RBAddClassCommentTransformation >> privateTransform [

	self definingClass comment: comment
]

{ #category : 'storing' }
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
