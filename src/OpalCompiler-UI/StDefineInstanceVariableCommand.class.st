Class {
	#name : 'StDefineInstanceVariableCommand',
	#superclass : 'StEditorCommand',
	#category : 'OpalCompiler-UI',
	#package : 'OpalCompiler-UI'
}

{ #category : 'default' }
StDefineInstanceVariableCommand class >> defaultName [
	^ 'Define instance variable'
]

{ #category : 'default' }
StDefineInstanceVariableCommand class >> defaultShortcut [
	^ $i meta shift
]

{ #category : 'executing' }
StDefineInstanceVariableCommand >> execute [
	| node reparator |
	node := self selectedNode.
	node ifNil: [ ^ self ].
	
	reparator := OCCodeReparator new
		node: node;
		requestor: self context;
		yourself.
	reparator declareInstVarWithoutMenu: node name
]