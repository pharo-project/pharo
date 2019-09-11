"
SUnit tests for forked processes in test resources
"
Class {
	#name : #TestResourceWithForkedProcessTestCase,
	#superclass : #TestCase,
	#category : #'SUnit-Tests-Core'
}

{ #category : #accessing }
TestResourceWithForkedProcessTestCase class >> resources [

	"Answer the TestResource class having a forked process"

	^ Array with: WithForkedProcessTestResource
]

{ #category : #tests }
TestResourceWithForkedProcessTestCase >> testFirst [

	"Test whether the TestResource's forked process is not terminated.
	A second test method will do the same and thereby validate that forked processes
	of a TestResource do not get terminated (in between tests)."

	self
		assert: WithForkedProcessTestResource current forkedProcess isTerminated not
		description: 'A forked process within a TestResource should not be terminated'
]

{ #category : #tests }
TestResourceWithForkedProcessTestCase >> testSecond [

	"Test whether the TestResource's forked process is not terminated between tests"

	"Use the other test method's implementation"
	self testFirst
]
