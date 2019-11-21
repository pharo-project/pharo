"
The standard TestExecutionEnvironment kills all the processes forked during the test execution when the test stops normally or with an time-out exception. That makes debugging of tests that use side-processes very hard because if such process causes an error.

This execution environment does not clean the process so you need to take responsibility for them. 

Because the tests are usually executed in context of the Morphic process, the UI cannot react or update itself. Thet makes debugging hard when the tests is trying to print something on to Transcript, needs to raise debugger because of error in a separate thread created by the test etc.

This test execution environment makes Morphic responsive during the execution of the test. It uses a separate process that is terminated at the end of the test execution.

-------------------------------------------------------------------
  Use this testing environment only during debugging of the tests
-------------------------------------------------------------------

Too use me, define in the TestCase a method named ""executionEnvironmentClass""
"
Class {
	#name : #TestExecutionEnvironmentWithWorldStepping,
	#superclass : #TestExecutionEnvironmentWithoutCleaning,
	#instVars : [
		'worldUpdateProcess'
	],
	#category : #'Morphic-Tests'
}

{ #category : #controlling }
TestExecutionEnvironmentWithWorldStepping >> runTestCase: aTestCase [

	worldUpdateProcess := [ 
		[ WorldMorph doOneCycle. Processor yield. false ] repeat.
	] forkNamed: 'TestExecutionEnvironment world updating process'.

	[ 
		super runTestCase: aTestCase.
	] ensure: [ 
		worldUpdateProcess terminate.
		worldUpdateProcess := nil ].
]
