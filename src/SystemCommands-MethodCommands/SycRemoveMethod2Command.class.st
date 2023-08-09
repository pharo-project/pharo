Class {
	#name : #SycRemoveMethod2Command,
	#superclass : #SycMethodCommand,
	#instVars : [
		'removeStrategy'
	],
	#category : #'SystemCommands-MethodCommands'
}

{ #category : #activation }
SycRemoveMethod2Command class >> methodMenuActivation [
	<classAnnotation>

	^ CmdContextMenuActivation byRootGroupItemOrder: 10000 for: ClyMethod asCalypsoItemContext
]

{ #category : #activation }
SycRemoveMethod2Command class >> methodShortcutActivation [
	<classAnnotation>

	^CmdShortcutActivation removalFor: ClyMethod asCalypsoItemContext
]

{ #category : #accessing }
SycRemoveMethod2Command >> defaultMenuIconName [
	^#removeIcon
]

{ #category : #accessing }
SycRemoveMethod2Command >> defaultMenuItemName [
	^'Remove refactoring'
]

{ #category : #execution }
SycRemoveMethod2Command >> execute [

	removeStrategy removeMethods: methods
]

{ #category : #initialization }
SycRemoveMethod2Command >> initialize [
	
	super initialize.

	removeStrategy := SycSilentlyRemoveMethodStrategy new
]

{ #category : #execution }
SycRemoveMethod2Command >> prepareFullExecutionInContext: aToolContext [
	super prepareFullExecutionInContext: aToolContext.
	(RBRemoveMethodDriver new scopes: aToolContext refactoringScopes methods: methods for: aToolContext lastSelectedClass) runRefactoring

]

{ #category : #accessing }
SycRemoveMethod2Command >> removeStrategy [
	^ removeStrategy
]

{ #category : #accessing }
SycRemoveMethod2Command >> removeStrategy: anObject [
	removeStrategy := anObject
]
