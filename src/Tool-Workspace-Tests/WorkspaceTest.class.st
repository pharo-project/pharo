"
SUnit tests for Workspace
"
Class {
	#name : #WorkspaceTest,
	#superclass : #TestCase,
	#category : #'Tool-Workspace-Tests-Base'
}

{ #category : #tests }
WorkspaceTest >> testEmptyContent [
	self assertEmpty: Workspace new contents
]
