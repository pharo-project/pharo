"
I know the items to create a testSuite that will be analysed by a DrTestsPlugin.
"
Class {
	#name : 'DTCommentTestConfiguration',
	#superclass : 'DTPluginConfiguration',
	#category : 'DrTests-CommentsToTests-Base',
	#package : 'DrTests-CommentsToTests',
	#tag : 'Base'
}

{ #category : 'converting' }
DTCommentTestConfiguration >> asTestSuite [

	| suite classes methods |
	suite := TestSuite named: 'Test Generated From Comments'.
	classes := self items addAll: (self items collect: [ :each | each class ]); yourself.
	methods := classes flatCollect: [ :each | each methods ].
	"keep only methods defined in a selected package or methods whose classes are defined in a selected package."
	methods := methods select: [ :m | (packagesSelected includes: m package) or: [ packagesSelected includes: m origin package ] ].
	methods do: [ :method |
		method pharoDocCommentNodes do: [ :docComment |
			suite addTest: (CommentTestCase for: docComment) ] ].
	^ suite
]
