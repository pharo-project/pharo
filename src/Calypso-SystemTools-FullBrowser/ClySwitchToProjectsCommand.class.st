"
I am a command to switch full browser to the projects mode.

Projects mode is not really supported. It requires plugin which provides package manager implementation.
There is Cargo package manager. But it is not ready yet.
Normally we should implement Metacello integration.
"
Class {
	#name : 'ClySwitchToProjectsCommand',
	#superclass : 'ClySwitchPackageViewModeCommand',
	#category : 'Calypso-SystemTools-FullBrowser-Commands-Packages',
	#package : 'Calypso-SystemTools-FullBrowser',
	#tag : 'Commands-Packages'
}

{ #category : 'testing' }
ClySwitchToProjectsCommand class >> canBeExecutedInContext: aContext [

	^ ClySystemEnvironment currentImage projectManager hasProjects
]

{ #category : 'accessing' }
ClySwitchToProjectsCommand >> defaultMenuItemName [
	^'Projects'
]

{ #category : 'accessing' }
ClySwitchToProjectsCommand >> description [

	^ 'Click to group packages by projects. Projects are currently defined by baselines.'
]

{ #category : 'execution' }
ClySwitchToProjectsCommand >> execute [

	^ browser switchToProjects
]

{ #category : 'testing' }
ClySwitchToProjectsCommand >> isAppliedToBrowser [

	^ (browser packageView showsItemsFromQuery: ClyAllProjectsQuery) and: [ browser isScopedModeEnabled not ]
]
