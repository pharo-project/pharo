"
SUnit tests for Workspace
"
Class {
	#name : #WorkspaceTest,
	#superclass : #TestCase,
	#category : #Tools-Test-Base
}

{ #category : #tests }
WorkspaceTest >> testEmptyContent [

	self assert: (Workspace new contents isEmpty)
]
