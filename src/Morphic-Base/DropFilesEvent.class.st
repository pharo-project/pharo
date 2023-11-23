"
This event is sent when an external file is dropped into the image itself.

The event can be used by any Morph implementing:

#wantsDropFiles: anEvent 
#dropFiles: anEvent 




"
Class {
	#name : 'DropFilesEvent',
	#superclass : 'DropEvent',
	#instVars : [
		'fileNames'
	],
	#category : 'Morphic-Base-Events',
	#package : 'Morphic-Base',
	#tag : 'Events'
}

{ #category : 'accessing' }
DropFilesEvent >> fileNames [
	^ fileNames
]

{ #category : 'accessing' }
DropFilesEvent >> fileNames: anObject [
	fileNames := anObject
]

{ #category : 'dispatching' }
DropFilesEvent >> requestDropReference: i [

	| fileName |
	fileName := self fileNames at: i.
	^ fileName asFileReference
]

{ #category : 'dispatching' }
DropFilesEvent >> sentTo: anObject [
	"Dispatch the receiver into anObject"
	^anObject handleDropFiles: self
]

{ #category : 'accessing' }
DropFilesEvent >> type [
	^#dropFilesEvent
]
