"
I know the items to create a testSuite that will be analysed by a DrTestsPlugin.
"
Class {
	#name : #DTCommentTestConfiguration,
	#superclass : #DTPluginConfiguration,
	#category : #'DrTests-CommentsToTests'
}

{ #category : #converting }
DTCommentTestConfiguration >> asTestSuite [
	| suite methods |
	suite := TestSuite new.
	suite := TestSuite named: 'Test Generated From Comments'.
	methods := (self items reject: [ :each | each isAbstract ]) flatCollect: [ :each | each methods ].
	methods := methods select: [:each | each sourceCode includesSubstring: '>>>' ].
	methods do: [ :m | m comments do: [ :com | 
					(com includesSubstring: '>>>')
						ifTrue: [ suite addTest: 
							(CommentTestCase comment: com class: m methodClass selector: m selector) ] ] ].
	^ suite
]
