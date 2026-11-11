"
I am a command to push down given variables
"
Class {
	#name : 'SycPushDownVariableCommand',
	#superclass : 'SycRefactorVariableCommand',
	#category : 'SystemCommands-VariableCommands',
	#package : 'SystemCommands-VariableCommands'
}

{ #category : 'execution' }
SycPushDownVariableCommand >> executeRefactoring [

	^ ((RePushDownInstanceVariableDriver newApplication: self application)
		scopes: refactoringScopes;
		variables: variables) runRefactoring
]

{ #category : 'accessing' }
SycPushDownVariableCommand >> fullBrowserOrder [

	^ 2001
]

{ #category : 'accessing' }
SycPushDownVariableCommand >> icon [

	^ self iconNamed: #down
]

{ #category : 'accessing' }
SycPushDownVariableCommand >> name [

	^ 'Push down'
]

{ #category : 'accessing' }
SycPushDownVariableCommand >> order [

	^ 1.4
]

{ #category : 'execution' }
SycPushDownVariableCommand >> prepareFullExecution [

	super prepareFullExecution.
	(variables allSatisfy: [ :selectedVar | selectedVar definingClass hasSubclasses ]) ifTrue: [ ^ self ].
	self inform: 'One of the selected variables belongs to a class that has no subclasses to push down the selection(s)'.
	CmCommandAborted signal
]
