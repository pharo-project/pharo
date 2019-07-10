"
I am a command to browse given variable.
For instance or class variable I just open browser on defining class.
For global variable I open browser of class of variable value
"
Class {
	#name : #SycBrowseVariableCommand,
	#superclass : #SycVariableCommand,
	#category : #'SystemCommands-VariableCommands'
}

{ #category : #accessing }
SycBrowseVariableCommand >> defaultMenuIconName [
	^#glamorousBrowse
]

{ #category : #accessing }
SycBrowseVariableCommand >> defaultMenuItemName [
	^'Browse'
]

{ #category : #execution }
SycBrowseVariableCommand >> execute [

	variables last openBrowser
]
