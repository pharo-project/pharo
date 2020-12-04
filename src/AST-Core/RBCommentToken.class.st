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
				 stop: aStopPosition.
]

{ #category : #testing }
RBCommentToken >> first [

	^ self start
]

{ #category : #testing }
RBCommentToken >> isComment [
	^true
]

{ #category : #testing }
RBCommentToken >> last [

	^ self stop
]

{ #category : #testing }
RBCommentToken >> stop [
	^ stopPosition
]

{ #category : #testing }
RBCommentToken >> stop: aPosition [
	stopPosition := aPosition
]
