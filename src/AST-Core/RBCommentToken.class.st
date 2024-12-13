"
First class representation of a scanned comment
"
Class {
	#name : #RBCommentToken,
	#superclass : #RBValueToken,
	#instVars : [
		'stopPosition'
	],
	#category : #'AST-Core-Tokens'
}

{ #category : #'instance creation' }
RBCommentToken class >> value: aString start: aStartPosition stop: aStopPosition [
	^self new value: aString;
		   		 start: aStartPosition;
				 stop: aStopPosition
]

{ #category : #accessing }
RBCommentToken >> first [

	^ self start
]

{ #category : #testing }
RBCommentToken >> isComment [
	^true
]

{ #category : #accessing }
RBCommentToken >> last [

	^ self stop
]

{ #category : #accessing }
RBCommentToken >> stop [
	^ stopPosition
]

{ #category : #accessing }
RBCommentToken >> stop: aPosition [
	stopPosition := aPosition
]
