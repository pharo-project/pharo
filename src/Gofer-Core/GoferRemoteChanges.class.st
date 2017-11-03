"
Handling remote changes
"
Class {
	#name : #GoferRemoteChanges,
	#superclass : #GoferChanges,
	#category : #Gofer-Core
}

{ #category : #queries }
GoferRemoteChanges >> sourceSnapshotOf: aReference [
	^ aReference workingCopy package snapshot
]

{ #category : #private }
GoferRemoteChanges >> targetSnapshotOf: aReference [
	^ (aReference resolveWith: self gofer) version snapshot
]
