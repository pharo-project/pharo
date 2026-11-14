"
I am a command to browse given variable.
For instance or class variable I just open browser on defining class.
For global variable I open browser of class of variable value
"
Class {
	#name : 'SycBrowseVariableCommand',
	#superclass : 'SycVariableNavigationCommand',
	#category : 'SystemCommands-VariableCommands',
	#package : 'SystemCommands-VariableCommands'
}

{ #category : 'testing' }
SycBrowseVariableCommand >> canBeExecuted [
	"The full browser variables menu already has its own Browse entry."

	^ self context isFullBrowserVariableContext not and: [ super canBeExecuted ]
]

{ #category : 'execution' }
SycBrowseVariableCommand >> executeRefactoring [

	variables last openBrowser
]

{ #category : 'accessing' }
SycBrowseVariableCommand >> iconName [
	^#browse
]

{ #category : 'accessing' }
SycBrowseVariableCommand >> name [
	^'Browse'
]
