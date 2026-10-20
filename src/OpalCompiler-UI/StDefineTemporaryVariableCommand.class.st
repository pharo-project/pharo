Class {
	#name : 'StDefineTemporaryVariableCommand',
	#superclass : 'StEditorCommand',
	#category : 'OpalCompiler-UI',
	#package : 'OpalCompiler-UI'
}

{ #category : 'default' }
StDefineTemporaryVariableCommand class >> defaultName [
	^ 'Define temporary variable'
]

{ #category : 'default' }
StDefineTemporaryVariableCommand class >> defaultShortcut [
	^ $t meta shift
]

{ #category : 'executing' }
StDefineTemporaryVariableCommand >> execute [
	| node reparator |
	node := self selectedNode.
	node ifNil: [ ^ self ].
	
	reparator := OCCodeReparator new
		node: node;
		requestor: self context;
		yourself.
	reparator declareTempWithoutMenu: node name
]