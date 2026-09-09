"
I am a command to inspect given variable.

"
Class {
	#name : 'SycInspectVariableCommand',
	#superclass : 'SycVariableCommand',
	#category : 'SystemCommands-VariableCommands',
	#package : 'SystemCommands-VariableCommands'
}

{ #category : 'activation' }
SycInspectVariableCommand class >> browserContextMenuActivation [

	<classAnnotation>
	^ CmdContextMenuActivation byItemOf: CmdExtraMenuGroup for: ClyVariableMethodGroup asCalypsoItemContext
]

{ #category : 'accessing' }
SycInspectVariableCommand >> defaultMenuIconName [

	^ #inspect
]

{ #category : 'accessing' }
SycInspectVariableCommand >> defaultMenuItemName [
	^'Inspect variable'
]

{ #category : 'accessing' }
SycInspectVariableCommand >> description [

	^ 'Inspect variable'
]

{ #category : 'execution' }
SycInspectVariableCommand >> execute [

	variables last openInspector 
]
