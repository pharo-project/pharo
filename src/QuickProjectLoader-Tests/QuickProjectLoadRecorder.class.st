"
I am a project entry that runs a block instead of loading anything, so tests never reach the network.
"
Class {
	#name : 'QuickProjectLoadRecorder',
	#superclass : 'QuickProjectEntry',
	#instVars : [
		'onLoad',
		'defaultBranch'
	],
	#category : 'QuickProjectLoader-Tests',
	#package : 'QuickProjectLoader-Tests'
}

{ #category : 'accessing' }
QuickProjectLoadRecorder >> defaultBranch: aString [

	defaultBranch := aString
]

{ #category : 'private' }
QuickProjectLoadRecorder >> fetchDefaultBranch [

	^ defaultBranch
]

{ #category : 'actions' }
QuickProjectLoadRecorder >> load [

	onLoad value
]

{ #category : 'accessing' }
QuickProjectLoadRecorder >> onLoad: aBlock [

	onLoad := aBlock
]
