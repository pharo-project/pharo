"
I am a command to add protocol scope to listScopes
"
Class {
	#name : #SycAddProtocolScopeCommand,
	#superclass : #SycProtocolCmCommand,
	#instVars : [
		'class'
	],
	#category : #'SystemCommands-MethodCommands'
}

{ #category : #execution }
SycAddProtocolScopeCommand >> execute [
	| scope protocols |
	self prepareFullExecution.
	protocols := methodGroups select: [:each | each isKindOf: ClyTaggedMethodGroup]
		thenCollect: [ :each | each tag].
	scope := RBBrowserEnvironment new forClass: class protocols: protocols.
	RefactoringSettings addScope: scope
]

{ #category : #accessing }
SycAddProtocolScopeCommand >> icon [
	^ self iconNamed: #add
]

{ #category : #accessing }
SycAddProtocolScopeCommand >> name [
	^ 'Add protocols to scope list'
]

{ #category : #execution }
SycAddProtocolScopeCommand >> prepareFullExecution [
	super prepareFullExecution.
	class := context lastSelectedClass
]
