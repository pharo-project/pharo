"
I am a command to switch full browser to the method group mode.
In that state browser shows method tags (protocol) and other virtual groups
"
Class {
	#name : #ClySwitchToMethodGroupsCommand,
	#superclass : #ClySwitchMethodViewModeCommand,
	#category : #'Calypso-SystemTools-FullBrowser-Commands-MethodGroups'
}

{ #category : #controling }
ClySwitchToMethodGroupsCommand >> execute [
	browser switchToMethodGroups
]

{ #category : #accessing }
ClySwitchToMethodGroupsCommand >> methodGroupType [
	^ClyMethodGroup
]
