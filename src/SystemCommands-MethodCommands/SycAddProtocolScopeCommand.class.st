"
I am a command to add protocol scope to listScopes
"
Class {
	#name : 'SycAddProtocolScopeCommand',
	#superclass : 'SycProtocolCmCommand',
	#instVars : [
		'class'
	],
	#category : 'SystemCommands-MethodCommands',
	#package : 'SystemCommands-MethodCommands'
}

{ #category : 'execution' }
SycAddProtocolScopeCommand >> execute [

	| scope protocols |
	self halt.
	self prepareFullExecution.
	protocols := methodGroups
		             select: [ :methodGroup | methodGroup isKindOf: ClyMethodsInProtocolGroup ]
		             thenCollect: [ :methodsInProtocolGroup | methodsInProtocolGroup protocol ].
	scope := RBBrowserEnvironment new forClass: class protocols: protocols.
	ScopesManager addScope: scope
]

{ #category : 'accessing' }
SycAddProtocolScopeCommand >> icon [
	^ self iconNamed: #add
]

{ #category : 'accessing' }
SycAddProtocolScopeCommand >> name [
	^ 'Add protocols to scope list'
]

{ #category : 'execution' }
SycAddProtocolScopeCommand >> prepareFullExecution [
	super prepareFullExecution.
	class := context lastSelectedClass
]
