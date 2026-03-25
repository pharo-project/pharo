"
An AnnouncerTestRunnerTest is a test class for testing the behavior of AnnouncerTestRunner
"
Class {
	#name : 'AnnouncerTestRunnerTest',
	#superclass : 'NewTestRunnerTest',
	#category : 'TestRunner-Tests',
	#package : 'TestRunner-Tests'
}

{ #category : 'testing' }
AnnouncerTestRunnerTest class >> shouldInheritSelectors [ 

	^ true
]

{ #category : 'tests' }
AnnouncerTestRunnerTest >> classForRunner [ 
	
	^ AnnouncerTestRunner 
]
