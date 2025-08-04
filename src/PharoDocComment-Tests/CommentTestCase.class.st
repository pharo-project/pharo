"
I am a TestCase created from a comment so my test `testIt` is not run normally. 
To create a new instance you can call one of my factory methods, if you want to understand it better you can look at the example in my class side.

## Explanation 
We want to run one test for each doc comment, therefore we need to have a testCase for those and this is this class.
The class itself is not a test in itself, but it is instantiated for each DocComment.
it is used by DrTest when you check all doc comments, and it is used by DocCommentsReleaseTest to check that all doc comments are correct. (see #buildSuite)



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
	"May be using double dispatch would be better
	so that the application of DrTest can open tools 
	without relying on this global."

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
	"pay attention this test is not meant to be executed normally. 
	It is part of the infrastructure to turn an expression into a test."
	| value |
	value := self evaluate.
	self assert: value isAssociation.
	self assert: value key equals: value value
]
