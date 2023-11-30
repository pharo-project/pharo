"
First class representation of a scanned comment
"
Class {
	#name : 'ASTCommentToken',
	#superclass : 'ASTValueToken',
	#instVars : [
		'stopPosition'
	],
	#category : 'AST-Core-Tokens',
	#package : 'AST-Core',
	#tag : 'Tokens'
}

{ #category : 'class initialization' }
ASTCommentToken class >> initialize [

	self deprecatedAliases: { #ASTCommentToken }
]

{ #category : 'instance creation' }
ASTCommentToken class >> value: aString start: aStartPosition stop: aStopPosition [
	^self new value: aString;
		   		 start: aStartPosition;
				 stop: aStopPosition
]

{ #category : 'accessing' }
ASTCommentToken >> first [

	^ self start
]

{ #category : 'testing' }
ASTCommentToken >> isComment [
	^true
]

{ #category : 'accessing' }
ASTCommentToken >> last [

	^ self stop
]

{ #category : 'accessing' }
ASTCommentToken >> stop [
	^ stopPosition
]

{ #category : 'accessing' }
ASTCommentToken >> stop: aPosition [
	stopPosition := aPosition
]
