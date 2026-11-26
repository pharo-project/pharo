"
I am raised when the configuration of a view changes
"
Class {
	#name : 'RubConfigurationChange',
	#superclass : 'RubAnnouncement',
	#instVars : [
		'configurationBlock'
	],
	#category : 'Morphic-Widgets-Editing-Announcements',
	#package : 'Morphic-Widgets',
	#tag : 'Editing-Announcements'
}

{ #category : 'accessing' }
RubConfigurationChange >> configurationBlock [
	^ configurationBlock ifNil: [ configurationBlock := [ :scrolledTextMorph | ] ]
]

{ #category : 'accessing' }
RubConfigurationChange >> configurationBlock: aBlockWithAScrolledTextMorphAsArgument [
	configurationBlock := aBlockWithAScrolledTextMorphAsArgument
]
