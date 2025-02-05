Class {
	#name : 'SycRemoveMethodCommand',
	#superclass : 'SycMethodCommand',
	#instVars : [
		'toolContext'
	],
	#category : 'SystemCommands-MethodCommands',
	#package : 'SystemCommands-MethodCommands'
}

{ #category : 'activation' }
SycRemoveMethodCommand class >> methodMenuActivation [
	<classAnnotation>

	^ CmdContextMenuActivation byRootGroupItemOrder: 10000 for: ClyMethod asCalypsoItemContext
]

{ #category : 'activation' }
SycRemoveMethodCommand class >> methodShortcutActivation [
	<classAnnotation>

	^CmdShortcutActivation removalFor: ClyMethod asCalypsoItemContext
]

{ #category : 'accessing' }
SycRemoveMethodCommand >> defaultMenuIconName [
	^#remove
]

{ #category : 'accessing' }
SycRemoveMethodCommand >> defaultMenuItemName [
	^'(R) Remove'
]

{ #category : 'execution' }
SycRemoveMethodCommand >> execute [
	
	(ReRemoveMethodDriver new
				 scopes: toolContext refactoringScopes
				 methods: methods) runRefactoring
]

{ #category : 'execution' }
SycRemoveMethodCommand >> prepareFullExecutionInContext: aToolContext [
	super prepareFullExecutionInContext: aToolContext.
	toolContext := aToolContext.
	"(RBRemoveMethodDriver new scopes: aToolContext refactoringScopes methods: methods for: aToolContext lastSelectedClass) runRefactoring"

]
