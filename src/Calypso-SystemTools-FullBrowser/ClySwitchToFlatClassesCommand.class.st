"
I am a command to switch the full browser to flat class mode
"
Class {
	#name : 'ClySwitchToFlatClassesCommand',
	#superclass : 'ClySwitchClassViewModeCommand',
	#category : 'Calypso-SystemTools-FullBrowser-Commands-Hierarchy',
	#package : 'Calypso-SystemTools-FullBrowser',
	#tag : 'Commands-Hierarchy'
}

{ #category : 'execution' }
ClySwitchToFlatClassesCommand >> execute [

	browser switchToFlatClasses
]

{ #category : 'testing' }
ClySwitchToFlatClassesCommand >> isAppliedToBrowser [

	^browser showsFlatClasses
]

{ #category : 'accessing' }
ClySwitchToFlatClassesCommand >> name [
	^'Flat'
]
