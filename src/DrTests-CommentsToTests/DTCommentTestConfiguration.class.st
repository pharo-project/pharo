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
	| suite |
	suite := TestSuite new.
	suite := TestSuite named: 'Test Generated From Comments'.
	self items
		reject: [ :each | each isAbstract ]
		thenDo: [ :c | 
			c methods
				do: [ :m | 
					(m methodClass compiledMethodAt: m selector) comments
						do: [ :com | 
							(com includesSubstring: '>>>')
								ifTrue: [ suite addTest: (CommentTestCase comment: com class: c selector: m selector) ] ] ] ].
	^ suite
]
