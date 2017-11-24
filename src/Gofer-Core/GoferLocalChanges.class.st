"
Handling local changes
"
Class {
	#name : #GoferLocalChanges,
	#superclass : #GoferChanges,
	#category : #'Gofer-Core'
}

{ #category : #queries }
GoferLocalChanges >> sourceSnapshotOf: aReference [
	| ancestors reference |
	ancestors := aReference workingCopy ancestry ancestors.
	ancestors isEmpty ifTrue: [ ^ MCSnapshot new ].
	reference := GoferVersionReference name: ancestors first name.
	^ (reference resolveWith: self gofer) version snapshot
]

{ #category : #queries }
GoferLocalChanges >> targetSnapshotOf: aReference [
	^ aReference workingCopy package snapshot
]
