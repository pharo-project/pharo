"
I represent the declaration of a class comment in a file.  My contents are the class comment to import, and the comment reader is the object who knows the class where to put the class comment.

Sending me the message #import makes me install the comment into the class.
"
Class {
	#name : #ClassCommentChunk,
	#superclass : #BehaviorOwnedChunk,
	#instVars : [
		'stamp'
	],
	#category : #'CodeImport-Chunks'
}

{ #category : #'instance creation' }
ClassCommentChunk class >> contents: someContents behaviorName: behaviorName isMeta: aBoolean stamp: stamp [
	^self new
			contents: someContents;
			behaviorName: behaviorName;
			isMeta: aBoolean;
			stamp: stamp;
			yourself
]

{ #category : #visiting }
ClassCommentChunk >> accept: aVisitor [

	^ aVisitor visitClassCommentChunk: self
]

{ #category : #accessing }
ClassCommentChunk >> description [

	^ 'Comment (', behaviorName, ')', stamp asString
]

{ #category : #importing }
ClassCommentChunk >> importFor: aRequestor logSource: logSource [
	(self existsBehavior)
		ifFalse: [ self error: ('Cannot install comment in unexistent behavior {1}' format: { behaviorName asString } ) ].
	
	^self targetClass instanceSide
						classComment: contents
						stamp: stamp
]

{ #category : #testing }
ClassCommentChunk >> isClassCommentDeclaration [

	^ true
]

{ #category : #accessing }
ClassCommentChunk >> stamp [
	^ stamp
]

{ #category : #accessing }
ClassCommentChunk >> stamp: classCommentStamp [
	stamp := classCommentStamp
]
