"
I know the items to create a testSuite that will be analysed by a DrTestsPlugin.
"
Class {
	#name : #DTCommentTestConfiguration,
	#superclass : #DTPluginConfiguration,
	#category : #'DrTests-CommentsToTests-Base'
}

{ #category : #converting }
DTCommentTestConfiguration >> asTestSuite [

	| suite classes methods |
	suite := TestSuite named: 'Test Generated From Comments'.
	classes := self items addAll: (self items collect: [ :each | each class ]); yourself.
	methods := classes flatCollect: [ :each | each methods ].
	methods do: [ :method | 
		method pharoDocCommentNodes do: [ :docComment | 
			suite addTest: (CommentTestCase for: docComment) ] ].
	^ suite
]
