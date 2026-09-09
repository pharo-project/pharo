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

{ #category : 'accessing' }
SycPushDownVariableCommand >> defaultMenuIconName [
	^ #down
]

{ #category : 'accessing' }
SycPushDownVariableCommand >> defaultMenuItemName [
	^'Push down'
]

{ #category : 'execution' }
SycPushDownVariableCommand >> execute [

	^ ((RePushDownInstanceVariableDriver newApplication: self application)
		   scopes: toolContext refactoringScopes;
		   variables: variables) runRefactoring
]

{ #category : 'execution' }
SycPushDownVariableCommand >> prepareFullExecutionInContext: aToolContext [

	super prepareFullExecutionInContext: aToolContext.

	(aToolContext selectedVariables allSatisfy: [ :selectedVar | selectedVar definingClass hasSubclasses ]) ifTrue: [ ^ self ].
	self inform: 'One of the selected variables belongs to a class that has no subclasses to push down the selection(s)'.
	CmdCommandAborted signal
]
