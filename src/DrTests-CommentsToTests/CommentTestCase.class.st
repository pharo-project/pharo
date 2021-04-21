"
I am a TestCase created from a comment.
To create a new instance you can call one of my factory methods, if you want to understand it better you can look at the example in my class side.

"
Class {
	#name : #CommentTestCase,
	#superclass : #TestCase,
	#instVars : [
		'docCommentNode'
	],
	#category : #'DrTests-CommentsToTests-Model'
}

{ #category : #'instance creation' }
CommentTestCase class >> for: aDocComment [

	^self new docCommentNode: aDocComment
]

{ #category : #accessing }
CommentTestCase class >> testSelectors [
	"we remove the selectors to add back later one of them, depending if is is error or not"
	^ super testSelectors \ { #testIt }
]

{ #category : #private }
CommentTestCase >> cleanUpInstanceVariables [
	self class allInstVarNames
		do: [ :name | 
			(#('docCommentNode')
				includes: name)
				ifFalse: [ self instVarNamed: name put: nil ] ]
]

{ #category : #accessing }
CommentTestCase >> currentValue [
	^ docCommentNode expression evaluate
]

{ #category : #accessing }
CommentTestCase >> docCommentNode: aDocComment [

	docCommentNode := aDocComment.
	testSelector := #testIt.
]

{ #category : #accessing }
CommentTestCase >> drTestsBrowse [
 	docCommentNode browse
]

{ #category : #accessing }
CommentTestCase >> drTestsName [
	^ self asString
]

{ #category : #accessing }
CommentTestCase >> expectedValue [
	^ docCommentNode result evaluate
]

{ #category : #accessing }
CommentTestCase >> expression [
	^ docCommentNode sourceNode contents
]

{ #category : #printing }
CommentTestCase >> printString [
	^ docCommentNode sourceNode contents 
]

{ #category : #tests }
CommentTestCase >> testIt [
	self assert: self expectedValue equals: self currentValue
]
