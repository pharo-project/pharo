"
I am a refactory change for a class comment change. Creating a RBClass during a refactoring operation and 
setting its class comment will create a comment change. Executing this change will set the class comment of the
class with my change stamp.
"
Class {
	#name : #RBCommentChange,
	#superclass : #RBRefactoryClassChange,
	#instVars : [
		'comment'
	],
	#category : #'Refactoring-Changes'
}

{ #category : #'instance creation' }
RBCommentChange class >> comment: aString in: aClass [
	^ self new
		changeClass: aClass;
		comment: aString;
		yourself
]

{ #category : #converting }
RBCommentChange >> asUndoOperation [
	^ self copy
		comment: self changeClass organization classComment;
		yourself
]

{ #category : #accessing }
RBCommentChange >> changeClass: aBehavior [
	super changeClass: aBehavior.
	isMeta := false
]

{ #category : #printing }
RBCommentChange >> changeString [
	^ 'Comment ' , self displayClassName
]

{ #category : #accessing }
RBCommentChange >> comment [
	^ comment
]

{ #category : #accessing }
RBCommentChange >> comment: aString [ 
	comment := aString
]

{ #category : #private }
RBCommentChange >> primitiveExecute [
	self changeClass 
		classComment: comment
		stamp: self changeStamp.
	SystemAnnouncer uniqueInstance 
		classCommented: self changeClass
]

{ #category : #printing }
RBCommentChange >> printOn: aStream [ 
	aStream
		nextPutAll: self displayClassName;
		nextPutAll: ' classComment: ';
		print: (self comment copyReplaceAll: '!' with: '!!');
		nextPutAll: ' stamp: ';
		print: (self changeStamp);
		nextPutAll: '!'
]
