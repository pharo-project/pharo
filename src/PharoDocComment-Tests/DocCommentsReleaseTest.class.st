"
This test class builds a test suite from all the doc comments in the system.

See #buildSuite on the class side.
"
Class {
	#name : #DocCommentsReleaseTest,
	#superclass : #TestCase,
	#category : #'PharoDocComment-Tests-Base'
}

{ #category : #'building suites' }
DocCommentsReleaseTest class >> buildSuite [
	| suite methods |
	suite := TestSuite named: 'Test Generated From Comments'.
	"we do not run examples in tests (as they are used just for testing and might be broken by
	design"
	methods := self environment allMethods reject: [ :method | method methodClass isTestCase ].
	"for now skip all class side methods to be in sync with DrTests"
	methods := methods reject: [ :method | method methodClass isClassSide ].
	methods do: [ :method | 
		method pharoDocCommentNodes do: [ :docComment | 
			suite addTest: (CommentTestCase for: docComment) ] ].
	"make sure to clean up the AST Cache to allow the GC to clean up"
	ASTCache reset.
	^ suite
]
