"
I am a project entry that runs a block instead of loading anything, so tests never reach the network.
"
Class {
	#name : 'QuickProjectLoadRecorder',
	#superclass : 'QuickProjectEntry',
	#instVars : [
		'onLoad'
	],
	#category : 'QuickProjectLoader-Tests',
	#package : 'QuickProjectLoader-Tests'
}

{ #category : 'actions' }
QuickProjectLoadRecorder >> load [

	onLoad value
]

{ #category : 'accessing' }
QuickProjectLoadRecorder >> onLoad: aBlock [

	onLoad := aBlock
]
