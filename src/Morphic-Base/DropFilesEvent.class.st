"
This event is sent when an external file is dropped into the image itself.

The event can be used by any Morph implementing:

#wantsDropFiles: anEvent 
#dropFiles: anEvent 




"
Class {
	#name : #DropFilesEvent,
	#superclass : #DropEvent,
	#category : #Morphic-Base-Events
}

{ #category : #dispatching }
DropFilesEvent >> sentTo: anObject [
	"Dispatch the receiver into anObject"
	^anObject handleDropFiles: self
]

{ #category : #accessing }
DropFilesEvent >> type [
	^#dropFilesEvent
]
