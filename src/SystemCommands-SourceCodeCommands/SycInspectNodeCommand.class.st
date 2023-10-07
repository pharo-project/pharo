"
I'm a simple command to inspect any AST node.
"
Class {
	#name : 'SycInspectNodeCommand',
	#superclass : 'SycSourceCodeCommand',
	#category : 'SystemCommands-SourceCodeCommands',
	#package : 'SystemCommands-SourceCodeCommands'
}

{ #category : 'execution' }
SycInspectNodeCommand >> asRefactorings [

	self error: 'Not a refactoring'
]

{ #category : 'execution' }
SycInspectNodeCommand >> defaultMenuIconName [
	^ #smallInspectIt
]

{ #category : 'execution' }
SycInspectNodeCommand >> defaultMenuItemName [
	^'Inspect AST Node ' , sourceNode class name
]

{ #category : 'execution' }
SycInspectNodeCommand >> execute [
	sourceNode inspect
]
