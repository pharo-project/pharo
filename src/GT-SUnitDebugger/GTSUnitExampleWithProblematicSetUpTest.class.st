"
I am not a test! I provide usage examples for the SUnit debugger.
"
Class {
	#name : #GTSUnitExampleWithProblematicSetUpTest,
	#superclass : #TestCase,
	#category : #'GT-SUnitDebugger'
}

{ #category : #testing }
GTSUnitExampleWithProblematicSetUpTest class >> isAbstract [
	^ true
]

{ #category : #testing }
GTSUnitExampleWithProblematicSetUpTest class >> isUnitTest [
	^ false
]

{ #category : #running }
GTSUnitExampleWithProblematicSetUpTest >> setUp [
	super setUp. 
	42 messageThatIsNotUnderstood
]

{ #category : #testing }
GTSUnitExampleWithProblematicSetUpTest >> testWithErrorInSetup [
	self assert: true
]
