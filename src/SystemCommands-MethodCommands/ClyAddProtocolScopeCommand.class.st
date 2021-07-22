"
I am a command to add protocol scope to listScopes
"
Class {
	#name : #ClyAddProtocolScopeCommand,
	#superclass : #ClyMethodGroupCommand,
	#instVars : [
		'class'
	],
	#category : #'SystemCommands-MethodCommands-Scope'
}

{ #category : #accessing }
ClyAddProtocolScopeCommand >> defaultMenuIconName [
	^ #add
]

{ #category : #accessing }
ClyAddProtocolScopeCommand >> defaultMenuItemName [ 
	^ 'Add protocols to scope list'
]

{ #category : #execution }
ClyAddProtocolScopeCommand >> execute [
	| scope protocols |
	protocols := methodGroups select: [:each | each isKindOf: ClyTaggedMethodGroup] 
		thenCollect: [ :each | each tag].
	scope := RBBrowserEnvironment new forClass: class protocols: protocols.
	RefactoringSettings addScope: scope
]

{ #category : #execution }
ClyAddProtocolScopeCommand >> prepareFullExecutionInContext: aToolContext [
	super prepareFullExecutionInContext: aToolContext.
	class := aToolContext lastSelectedClass.
]
