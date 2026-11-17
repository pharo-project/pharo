"
I check that a deprecation reaching the test runner is an error of the test on the Pharo CI, without rewriting the code under test, and a warning elsewhere.
"
Class {
	#name : 'DeprecationOnCITest',
	#superclass : 'TestCase',
	#category : 'SUnit-Tests-Core',
	#package : 'SUnit-Tests',
	#tag : 'Core'
}

{ #category : 'helpers' }
DeprecationOnCITest >> exampleOnCI: aBoolean [

	^ (DeprecationOnCIExampleTest selector: #testSendsDeprecatedMessage)
		  onCI: aBoolean;
		  yourself
]

{ #category : 'tests' }
DeprecationOnCITest >> testDeprecationIsAWarningOutsideCI [
	"Outside the CI the deprecation goes on to the code that runs the test: here, this test, which resumes it so that it does not rewrite the example."

	| result |
	result := [ (self exampleOnCI: false) run ]
		          on: Deprecation
		          do: [ :deprecation | deprecation resume ].
	self assert: result passed size equals: 1.
	self assert: result errors isEmpty
]

{ #category : 'tests' }
DeprecationOnCITest >> testDeprecationIsAnErrorOnCI [

	| result |
	result := (self exampleOnCI: true) run.
	self assert: result errors size equals: 1.
	self assert: result passed isEmpty
]

{ #category : 'tests' }
DeprecationOnCITest >> testDeprecationIsNotRewrittenOnCI [

	(self exampleOnCI: true) run.
	self assert: (DeprecationOnCIExampleTest >> #testSendsDeprecatedMessage sendsSelector: #deprecatedMessage)
]
