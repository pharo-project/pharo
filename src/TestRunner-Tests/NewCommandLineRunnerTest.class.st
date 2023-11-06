Class {
	#name : 'NewCommandLineRunnerTest',
	#superclass : 'NewTestRunnerTest',
	#category : 'TestRunner-Tests-Core',
	#package : 'TestRunner-Tests',
	#tag : 'Core'
}

{ #category : 'testing' }
NewCommandLineRunnerTest class >> shouldInheritSelectors [
	^ true
]

{ #category : 'running' }
NewCommandLineRunnerTest >> setUp [

	super setUp.
	runner := NewCommandLineRunner new
]

{ #category : 'tests' }
NewCommandLineRunnerTest >> testReporterDatasShouldBeResetBeetwenTwoExecution [

	runner reporter: XmlReporter new.
	runner run: GreenTest.
	self assert: runner reporter datasAreReseted
]
