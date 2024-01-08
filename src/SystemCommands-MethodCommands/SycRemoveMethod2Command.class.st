Class {
	#name : 'SycRemoveMethod2Command',
	#superclass : 'SycMethodCommand',
	#instVars : [
		'toolContext'
	],
	#category : 'SystemCommands-MethodCommands',
	#package : 'SystemCommands-MethodCommands'
}

{ #category : 'activation' }
SycRemoveMethod2Command class >> methodMenuActivation [
	<classAnnotation>

	^ CmdContextMenuActivation byRootGroupItemOrder: 10000 for: ClyMethod asCalypsoItemContext
]

{ #category : 'activation' }
SycRemoveMethod2Command class >> methodShortcutActivation [
	<classAnnotation>

	^CmdShortcutActivation removalFor: ClyMethod asCalypsoItemContext
]

{ #category : 'accessing' }
SycRemoveMethod2Command >> defaultMenuIconName [
	^#remove
]

{ #category : 'accessing' }
SycRemoveMethod2Command >> defaultMenuItemName [
	^'(R) Remove'
]

{ #category : 'execution' }
SycRemoveMethod2Command >> execute [
	
	(ReRemoveMethodDriver new
				 scopes: toolContext refactoringScopes
				 methods: methods) runRefactoring
]

{ #category : 'execution' }
SycRemoveMethod2Command >> isComplexRefactoring [ 

	^ false
]

{ #category : 'execution' }
SycRemoveMethod2Command >> prepareFullExecutionInContext: aToolContext [
	super prepareFullExecutionInContext: aToolContext.
	toolContext := aToolContext.
	"(RBRemoveMethodDriver new scopes: aToolContext refactoringScopes methods: methods for: aToolContext lastSelectedClass) runRefactoring"

]
