"
I am a command to push down given variables
"
Class {
	#name : 'SycPushDownVariableCommand',
	#superclass : 'SycRefactorVariableCommand',
	#category : 'SystemCommands-VariableCommands',
	#package : 'SystemCommands-VariableCommands'
}

{ #category : 'activation' }
SycPushDownVariableCommand class >> sourceCodeMenuActivation [
	<classAnnotation>

	^SycSourceCodeMenuActivation byRootGroupItemOrder: 1.4 for: ClySourceCodeContext
]

{ #category : 'execution' }
SycPushDownVariableCommand >> asRefactorings [

	^self
		createRefactorings: RBPushDownInstanceVariableRefactoring
]

{ #category : 'accessing' }
SycPushDownVariableCommand >> defaultMenuIconName [
	^ #down
]

{ #category : 'accessing' }
SycPushDownVariableCommand >> defaultMenuItemName [
	^'Push down'
]

{ #category : 'execution' }
SycPushDownVariableCommand >> prepareFullExecutionInContext: aToolContext [
	"Private - If one of the selected variables belongs to a leaf class, without subclasses, abort the command"
	
	super prepareFullExecutionInContext: aToolContext.

	(aToolContext selectedVariables allSatisfy: [ : selectedVar | selectedVar definingClass hasSubclasses ])
		ifFalse: [ 
			self inform: 'One of the selected variables belongs to a class that has no subclasses to push down the selection(s)'.
			CmdCommandAborted signal ]
		ifTrue: [ 
			| noUsers |
			noUsers := aToolContext confirmUnusedVariablesInDefiningClass: variables.
			noUsers ifFalse: [ CmdCommandAborted signal ] ].

]
