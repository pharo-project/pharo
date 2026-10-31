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
SycInspectNodeCommand >> execute [
	sourceNode inspect
]

{ #category : 'execution' }
SycInspectNodeCommand >> iconName [
	^ #smallInspectIt
]

{ #category : 'execution' }
SycInspectNodeCommand >> name [
	^'Inspect AST Node ' , sourceNode class name
]
