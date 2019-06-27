"
I am a command to switch full browser to the projects mode.

Projects mode is not really supported. It requires plugin which provides package manager implementation.
There is Cargo package manager. But it is not ready yet.
Normally we should implement Metacello integration.
"
Class {
	#name : #ClySwitchToProjectsCommand,
	#superclass : #ClySwitchPackageViewModeCommand,
	#category : #'Calypso-SystemTools-FullBrowser-Commands-Packages'
}

{ #category : #testing }
ClySwitchToProjectsCommand class >> canBeExecutedInContext: aContext [

	^ ClySystemEnvironment currentImage projectManager projectManagers notEmpty 
]

{ #category : #accessing }
ClySwitchToProjectsCommand >> defaultMenuItemName [
	^'Projects'
]

{ #category : #accessing }
ClySwitchToProjectsCommand >> description [
	^'Do not supported currently'
]

{ #category : #execution }
ClySwitchToProjectsCommand >> execute [
	browser switchToProjects
]

{ #category : #testing }
ClySwitchToProjectsCommand >> isAppliedToBrowser [

	^ browser isInProjectView
]
