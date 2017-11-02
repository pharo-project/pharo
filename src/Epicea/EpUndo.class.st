"
I represent a Undo operation performed in a EpLogBrowser.
"
Class {
	#name : #EpUndo,
	#superclass : #EpLogBrowserOperation,
	#category : 'Epicea-Model'
}

{ #category : #'instance creation' }
EpUndo class >> forAll: aCollectionOfEntryReferences [
	^ self basicNew
		initializeWith: aCollectionOfEntryReferences;
		yourself
]

{ #category : #visitor }
EpUndo >> accept: aChangeVisitor [
	^ aChangeVisitor visitUndo: self
]

{ #category : #copying }
EpUndo >> copyWithEntryReferences: aCollection [
	^ self class forAll: aCollection
]

{ #category : #accessing }
EpUndo >> title [
	^ 'Revert changes'
]
