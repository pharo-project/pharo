Class {
	#name : 'MCSnapshotBrowserCommand',
	#superclass : 'CmCommand',
	#category : 'MonticelloGUI-Commands',
	#package : 'MonticelloGUI',
	#tag : 'Commands'
}

{ #category : 'accessing - selection' }
MCSnapshotBrowserCommand >> selectedClass [

	^ context classSelection
]

{ #category : 'private' }
MCSnapshotBrowserCommand >> selectedClassIsLoaded [
	"Answer <true> if the currently selected class is loaded in the system"

	^ self class environment hasClassNamed: self selectedClass
]

{ #category : 'menu messages' }
MCSnapshotBrowserCommand >> tools [

	^ context application tools
]
