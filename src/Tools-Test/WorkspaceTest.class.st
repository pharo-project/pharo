"
SUnit tests for Workspace
"
Class {
	#name : #WorkspaceTest,
	#superclass : #TestCase,
	#category : #'Tools-Test-Base'
}

{ #category : #tests }
WorkspaceTest >> testEmptyContent [
	self assertEmpty: Workspace new contents
]
