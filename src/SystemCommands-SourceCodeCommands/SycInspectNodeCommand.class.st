"
I'm a simple command to inspect any AST node.
"
Class {
	#name : 'SycInspectNodeCommand',
	#superclass : 'SycSourceCodeCmCommand',
	#category : 'SystemCommands-SourceCodeCommands',
	#package : 'SystemCommands-SourceCodeCommands'
}

{ #category : 'testing' }
SycInspectNodeCommand >> canBeExecuted [

	^ self context lastSelectedSourceNode notNil
]

{ #category : 'accessing' }
SycInspectNodeCommand >> description [

	^ 'Inspects the selected AST node'
]

{ #category : 'execution' }
SycInspectNodeCommand >> execute [

	self context lastSelectedSourceNode inspect
]

{ #category : 'accessing' }
SycInspectNodeCommand >> icon [

	^ self iconNamed: #smallInspectIt
]

{ #category : 'accessing' }
SycInspectNodeCommand >> name [

	^ 'Inspect AST Node ', self context lastSelectedSourceNode class name
]

{ #category : 'accessing' }
SycInspectNodeCommand >> order [

	^ 1.4
]
