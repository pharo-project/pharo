Class {
	#name : #TestClassForTestingDebugger,
	#superclass : #TestCase,
	#category : #'GT-Tests-Debugger'
}

{ #category : #tests }
TestClassForTestingDebugger >> testBla [
<expectedFailure>
	self assert: self bla equals: 1.
]
