"
I am a command to inspect given variable.
For live variables lile globals or class variables I open inspector on variable value.
Also I open inspector on instance variable of class side because they also have live values.
For instance side instance variable I inspect slot definition instance.
"
Class {
	#name : 'SycInspectVariableCommand',
	#superclass : 'SycVariableCommand',
	#category : 'SystemCommands-VariableCommands',
	#package : 'SystemCommands-VariableCommands'
}

{ #category : 'testing' }
SycInspectVariableCommand class >> canBeExecutedInContext: aToolContext [
	(super canBeExecutedInContext: aToolContext) ifFalse: [ ^false ].

	^aToolContext lastSelectedVariable isLive
]

{ #category : 'accessing' }
SycInspectVariableCommand >> defaultMenuIconName [
	^#inspect
]

{ #category : 'accessing' }
SycInspectVariableCommand >> defaultMenuItemName [
	^'Inspect'
]

{ #category : 'accessing' }
SycInspectVariableCommand >> description [

	^ 'Inspect variable value'
]

{ #category : 'execution' }
SycInspectVariableCommand >> execute [

	variables last openInspector
]
