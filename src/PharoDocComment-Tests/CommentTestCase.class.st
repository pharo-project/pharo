"
I am a TestCase created from a comment.
To create a new instance you can call one of my factory methods, if you want to understand it better you can look at the example in my class side.

"
Class {
	#name : 'CommentTestCase',
	#superclass : 'TestCase',
	#instVars : [
		'docCommentNode'
	],
	#category : 'PharoDocComment-Tests-Base',
	#package : 'PharoDocComment-Tests',
	#tag : 'Base'
}

{ #category : 'instance creation' }
CommentTestCase class >> for: aDocComment [

	^self new docCommentNode: aDocComment
]

{ #category : 'accessing' }
CommentTestCase class >> testSelectors [
	"we remove the selectors to add back later one of them, depending if is is error or not"
	^ super testSelectors \ { #testIt }
]

{ #category : 'accessing' }
CommentTestCase >> docCommentNode: aDocComment [

	docCommentNode := aDocComment.
	testSelector := #testIt
]

{ #category : 'actions' }
CommentTestCase >> drTestsBrowse [
 	docCommentNode browse
]

{ #category : 'public access' }
CommentTestCase >> evaluate [

	^ docCommentNode expression evaluate
]

{ #category : 'accessing' }
CommentTestCase >> expression [
	^ docCommentNode sourceNode contents
]

{ #category : 'private' }
CommentTestCase >> instanceVariablesToKeep [

	^ #( 'docCommentNode' )
]

{ #category : 'printing' }
CommentTestCase >> printString [
	^ docCommentNode sourceNode contents
]

{ #category : 'tests' }
CommentTestCase >> testIt [

	| value |
	value := self evaluate.
	self assert: value isAssociation.
	self assert: value key equals: value value
]
