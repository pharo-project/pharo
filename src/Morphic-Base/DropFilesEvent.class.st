"
This event is sent when an external file is dropped into the image itself.

The event can be used by any Morph implementing:

#wantsDropFiles: anEvent 
#dropFiles: anEvent 




"
Class {
	#name : #DropFilesEvent,
	#superclass : #DropEvent,
	#instVars : [
		'fileNames'
	],
	#category : #'Morphic-Base-Events'
}

{ #category : #accessing }
DropFilesEvent >> fileNames [
	^ fileNames
]

{ #category : #accessing }
DropFilesEvent >> fileNames: anObject [
	fileNames := anObject
]

{ #category : #dispatching }
DropFilesEvent >> requestDropReference: i [

	| fileName |

	fileNames ifNil: [ 
		"The file names are not stored in the event, they need to be obtained by a VM primitive"
		^ FileReference requestDropReference: i ].
	
	"The event already knows the files (case of events transformed from OSWindow events)"
	fileName := self fileNames at: i.
	^ (FilePathEncoder decode: fileName) asFileReference


]

{ #category : #dispatching }
DropFilesEvent >> sentTo: anObject [
	"Dispatch the receiver into anObject"
	^anObject handleDropFiles: self
]

{ #category : #accessing }
DropFilesEvent >> type [
	^#dropFilesEvent
]
