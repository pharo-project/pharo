"
I am a refactory change for a class comment change. Creating a RBClass during a refactoring operation and 
setting its class comment will create a comment change. Executing this change will set the class comment of the
class with my change stamp.
"
Class {
	#name : 'ReCommentChange',
	#superclass : 'ReClassChange',
	#instVars : [
		'comment'
	],
	#category : 'Refactoring-Changes',
	#package : 'Refactoring-Changes'
}

{ #category : 'instance creation' }
ReCommentChange class >> comment: aString in: aClass [

	^ self new
		  changeClass: aClass;
		  comment: aString;
		  yourself
]

{ #category : 'converting' }
ReCommentChange >> asUndoOperation [

	^ self copy
		  comment: self changeClass comment;
		  yourself
]

{ #category : 'accessing' }
ReCommentChange >> changeClass: aBehavior [

	super changeClass: aBehavior.
	isMeta := false
]

{ #category : 'printing' }
ReCommentChange >> changeString [

	^ 'Comment ' , self displayClassName
]

{ #category : 'accessing' }
ReCommentChange >> comment [

	^ comment
]

{ #category : 'accessing' }
ReCommentChange >> comment: aString [

	comment := aString
]

{ #category : 'private' }
ReCommentChange >> generateChanges [

	self changeClass comment: comment stamp: self changeStamp.
	
]

{ #category : 'printing' }
ReCommentChange >> printOn: aStream [

	aStream
		nextPutAll: self displayClassName;
		nextPutAll: ' classComment: ';
		print: (self comment copyReplaceAll: '!' with: '!!');
		nextPutAll: ' stamp: ';
		print: self changeStamp
]
