"
I am a base class for the commands which switch the full browser mode of the class view.
It can be flat list or class hierarchy for example
"
Class {
	#name : 'ClySwitchClassViewModeCommand',
	#superclass : 'ClySwitchBrowserModeCommand',
	#category : 'Calypso-SystemTools-FullBrowser-Commands-Hierarchy',
	#package : 'Calypso-SystemTools-FullBrowser',
	#tag : 'Commands-Hierarchy'
}

{ #category : 'testing' }
ClySwitchClassViewModeCommand class >> isAbstract [
	^self = ClySwitchClassViewModeCommand
]

{ #category : 'testing' }
ClySwitchClassViewModeCommand >> canBeExecuted [

	^ self context isClassSelected
]
