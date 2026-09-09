"
I am a command to inspect given variable value.
For live variables lile globals or class variables I open inspector on variable value.
Also I open inspector on instance variable of class side because they also have live values.
"
Class {
	#name : 'SycInspectVariableValueCommand',
	#superclass : 'SycVariableCommand',
	#category : 'SystemCommands-VariableCommands',
	#package : 'SystemCommands-VariableCommands'
}

{ #category : 'testing' }
SycInspectVariableValueCommand class >> canBeExecutedInContext: aToolContext [
	(super canBeExecutedInContext: aToolContext) ifFalse: [ ^false ].

	^aToolContext lastSelectedVariable isLive
]

{ #category : 'accessing' }
SycInspectVariableValueCommand >> defaultMenuIconName [
	^#inspect
]

{ #category : 'accessing' }
SycInspectVariableValueCommand >> defaultMenuItemName [
	^'Inspect value'
]

{ #category : 'accessing' }
SycInspectVariableValueCommand >> description [

	^ 'Inspect variable value'
]

{ #category : 'execution' }
SycInspectVariableValueCommand >> execute [

	variables last openValueInspector
]
