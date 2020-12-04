"
I build the default command tree for the debugger toolbar presenter.
The command classes I instantiate are specified under the ""accessing - commands classes"" protocol.
The group names and descriptions are specified under the ""accessing"" protocol.

You can evolve the default groups names and descriptions and the default command classes, but you cannot extend me.
Using me to add command in the tree makes them default for the debugger toolbar presenter.
"
Class {
	#name : #StDebuggerToolbarCommandTreeBuilder,
	#superclass : #StDebuggerCommandTreeBuilder,
	#category : #'NewTools-Debugger-Model'
}

{ #category : #accessing }
StDebuggerToolbarCommandTreeBuilder class >> displayStrategy [
	^CmUIDisplayAsGroup
]

{ #category : #accessing }
StDebuggerToolbarCommandTreeBuilder class >> groupDescription [
	^ 'Debug actions'
]

{ #category : #accessing }
StDebuggerToolbarCommandTreeBuilder class >> groupName [ 	
	^'Toolbar'
]

{ #category : #building }
StDebuggerToolbarCommandTreeBuilder >> buildCommandGroupFrom: commandClasses named: commandGroupName description: commandGroupDescription [
	| group |
	group := (CmCommandGroup named: commandGroupName) asSpecGroup.
	group beDisplayedAsGroup.
	group description: commandGroupDescription.
	group iconName: #smallDebug.
	commandClasses
		do: [ :cmdClass | 
			group
				register: (self buildSpecCommand: cmdClass forContext: stDebuggerInstance) ].
	^ group
]

{ #category : #building }
StDebuggerToolbarCommandTreeBuilder >> buildDebuggerCommandGroup [
	|group|
	group := self group.
	group displayStrategy: self displayStrategy.
	group decoratedGroup register: self executionControlDebuggingCommands. 
	group decoratedGroup register: self contextRequestDebuggingCommands.
	group decoratedGroup register: self contextControlDebuggingCommands
	
]

{ #category : #'accessing - commands classes' }
StDebuggerToolbarCommandTreeBuilder >> contextControlCommandClasses [
	^ {StDefineMissingEntityCommand}
]

{ #category : #commands }
StDebuggerToolbarCommandTreeBuilder >> contextControlDebuggingCommands [
	^ self
		buildCommandGroupFrom: self contextControlCommandClasses
		named: self contextControlGroupName
		description: self contextControlGroupDescription
]

{ #category : #accessing }
StDebuggerToolbarCommandTreeBuilder >> contextControlGroupDescription [
	^'Actions performed on the interrupted context'
]

{ #category : #accessing }
StDebuggerToolbarCommandTreeBuilder >> contextControlGroupName [
	^'Context control'
]

{ #category : #'accessing - commands classes' }
StDebuggerToolbarCommandTreeBuilder >> contextRequestCommandClasses [
	^ {
	StWhereIsCommand
	}
]

{ #category : #commands }
StDebuggerToolbarCommandTreeBuilder >> contextRequestDebuggingCommands [
	^ self
		buildCommandGroupFrom: self contextRequestCommandClasses
		named: self contextRequestGroupName
		description: self contextRequestGroupDescription
]

{ #category : #accessing }
StDebuggerToolbarCommandTreeBuilder >> contextRequestGroupDescription [
	^ 'Requests on the interrupted context'
]

{ #category : #accessing }
StDebuggerToolbarCommandTreeBuilder >> contextRequestGroupName [
	^'Context requests'
]

{ #category : #accessing }
StDebuggerToolbarCommandTreeBuilder >> defaultDebugActionsDescription [
	^'Defaults debugging actions'
]

{ #category : #accessing }
StDebuggerToolbarCommandTreeBuilder >> defaultDebugActionsGroupName [
	^'Debugging actions'
]

{ #category : #'accessing - commands classes' }
StDebuggerToolbarCommandTreeBuilder >> executionControlCommandClasses [
	^ {
	StProceedCommand.
	StStepIntoCommand.
	StStepOverCommand.
	StStepThroughCommand.
	StRunToSelectionCommand.
	StRestartCommand.
	StReturnValueCommand
	}
]

{ #category : #commands }
StDebuggerToolbarCommandTreeBuilder >> executionControlDebuggingCommands [
	^ self
		buildCommandGroupFrom: self executionControlCommandClasses
		named: self executionControlGroupName
		description: self executionControlGroupDescription
]

{ #category : #accessing }
StDebuggerToolbarCommandTreeBuilder >> executionControlGroupDescription [
	^'Debug actions applied to the executed context'
]

{ #category : #accessing }
StDebuggerToolbarCommandTreeBuilder >> executionControlGroupName [
	^'Execution control'
]
