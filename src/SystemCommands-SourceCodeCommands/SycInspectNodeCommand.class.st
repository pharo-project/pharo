"
Inspects the selected AST node
"
Class {
	#name : 'SycInspectNodeCommand',
	#superclass : 'SycSourceCodeCommand',
	#category : 'SystemCommands-SourceCodeCommands',
	#package : 'SystemCommands-SourceCodeCommands'
}

{ #category : 'testing' }
SycInspectNodeCommand >> canBeExecuted [

	^ self context lastSelectedSourceNode notNil
]

{ #category : 'execution' }
SycInspectNodeCommand >> execute [

	self context lastSelectedSourceNode inspect
]

{ #category : 'accessing' }
SycInspectNodeCommand >> iconName [

	^ #smallInspectIt
]

{ #category : 'accessing' }
SycInspectNodeCommand >> name [

	^ 'Inspect AST Node ', self context lastSelectedSourceNode class name
]

{ #category : 'accessing' }
SycInspectNodeCommand >> order [

	^ 1.4
]
