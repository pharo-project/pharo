"
Handling changes
"
Class {
	#name : #GoferChanges,
	#superclass : #GoferWorking,
	#category : #'Gofer-Core'
}

{ #category : #private }
GoferChanges >> addReference: aReference [
	super addReference: aReference.
	self model operations 
		addAll: (self patchsetOf: aReference) operations
]

{ #category : #private }
GoferChanges >> defaultModel [
	^ MCPatch operations: OrderedCollection new
]

{ #category : #running }
GoferChanges >> execute [
	^ self model
]

{ #category : #queries }
GoferChanges >> patchsetOf: aReference [
	"Answer the source snapshot of aReference."
	
	| source target |
	source := self sourceSnapshotOf: aReference.
	target := self targetSnapshotOf: aReference.
	^ target patchRelativeToBase: source
]

{ #category : #queries }
GoferChanges >> sourceSnapshotOf: aReference [
	"Answer the source snapshot of aReference."
	
	self subclassResponsibility
]

{ #category : #queries }
GoferChanges >> targetSnapshotOf: aReference [
	"Answer the source snapshot of aReference."
	
	self subclassResponsibility
]
