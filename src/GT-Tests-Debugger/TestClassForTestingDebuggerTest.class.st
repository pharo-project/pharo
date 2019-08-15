Class {
	#name : #TestClassForTestingDebuggerTest,
	#superclass : #TestCase,
	#category : #'GT-Tests-Debugger'
}

{ #category : #tests }
TestClassForTestingDebuggerTest >> testBla [
<expectedFailure>
	self assert: self bla equals: 1.
]
