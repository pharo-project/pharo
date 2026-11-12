"
I am the root of the commands which browse what the selected variables refer to: their references, their writers, their value or their class.
Unlike the variable refactorings, I accept global variables.
In the variable pane my commands are ordered by #fullBrowserOrder, and I keep the tool context to open the browsers from it.
"
Class {
	#name : 'SycVariableNavigationCommand',
	#superclass : 'SycVariableCommand',
	#category : 'SystemCommands-VariableCommands',
	#package : 'SystemCommands-VariableCommands'
}

{ #category : 'execution' }
SycVariableNavigationCommand >> prepareFullExecution [

	variables := self context selectedVariables.
	toolContext := self context
]
