Class {
	#name : #TestClassForTestingDebuggerTest,
	#superclass : #TestCase,
	#category : #'GT-Tests-Debugger'
}

{ #category : #testing }
TestClassForTestingDebuggerTest class >> isDeprecated [
	^ true
]

{ #category : #tests }
TestClassForTestingDebuggerTest >> testBla [
<expectedFailure>
	self assert: self bla equals: 1.
]
