"
First class representation of a scanned comment
"
Class {
	#name : 'OCCommentToken',
	#superclass : 'OCValueToken',
	#instVars : [
		'stopPosition'
	],
	#category : 'AST-Core-Tokens',
	#package : 'AST-Core',
	#tag : 'Tokens'
}

{ #category : 'instance creation' }
OCCommentToken class >> value: aString start: aStartPosition stop: aStopPosition [
	^self new value: aString;
		   		 start: aStartPosition;
				 stop: aStopPosition
]

{ #category : 'accessing' }
OCCommentToken >> first [

	^ self start
]

{ #category : 'testing' }
OCCommentToken >> isComment [
	^true
]

{ #category : 'accessing' }
OCCommentToken >> last [

	^ self stop
]

{ #category : 'accessing' }
OCCommentToken >> stop [
	^ stopPosition
]

{ #category : 'accessing' }
OCCommentToken >> stop: aPosition [
	stopPosition := aPosition
]
