"
I am a command to switch full browser to the variables mode.
In that state browser shows variables in the third pane
"
Class {
	#name : #ClySwitchToVariablesCommand,
	#superclass : #ClySwitchMethodViewModeCommand,
	#category : #'Calypso-SystemTools-FullBrowser-Commands-MethodGroups'
}

{ #category : #controling }
ClySwitchToVariablesCommand >> execute [
	browser switchToVariables
]

{ #category : #accessing }
ClySwitchToVariablesCommand >> methodGroupType [
	^ClyVariable
]
