Class {
	#name : 'MCBrowserAbstractClassCommand',
	#superclass : 'MCSnapshotBrowserCommand',
	#category : 'MonticelloGUI-Commands',
	#package : 'MonticelloGUI',
	#tag : 'Commands'
}

{ #category : 'testing' }
MCBrowserAbstractClassCommand >> canBeExecuted [ 

	^ self selectedClass notNil
]
