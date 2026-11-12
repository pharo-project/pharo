"
I am a command to inspect given variable.

"
Class {
	#name : 'SycInspectVariableCommand',
	#superclass : 'ClyCommand',
	#category : 'SystemCommands-VariableCommands',
	#package : 'SystemCommands-VariableCommands'
}

{ #category : 'testing' }
SycInspectVariableCommand >> canBeExecuted [

	^ context isAboutBrowserItemType: ClyVariableMethodGroup
]

{ #category : 'accessing' }
SycInspectVariableCommand >> description [

	^ 'Inspect variable'
]

{ #category : 'execution' }
SycInspectVariableCommand >> executeRefactoring [

	self context selectedVariables last openInspector
]

{ #category : 'accessing' }
SycInspectVariableCommand >> iconName [

	^ #inspect
]

{ #category : 'accessing' }
SycInspectVariableCommand >> name [

	^ 'Inspect variable'
]

{ #category : 'accessing' }
SycInspectVariableCommand >> order [

	^ 1000
]
