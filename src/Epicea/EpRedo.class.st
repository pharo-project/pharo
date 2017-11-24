"
I represent a ReDo operation performed in a EpLogBrowser.
"
Class {
	#name : #EpRedo,
	#superclass : #EpLogBrowserOperation,
	#category : #'Epicea-Model'
}

{ #category : #'instance creation' }
EpRedo class >> forAll: aCollectionOfEntryReferences [
	^ self basicNew
		initializeWith: aCollectionOfEntryReferences;
		yourself
]

{ #category : #visitor }
EpRedo >> accept: aChangeVisitor [
	^ aChangeVisitor visitRedo: self
]

{ #category : #copying }
EpRedo >> copyWithEntryReferences: aCollection [
	^ self class forAll: aCollection
]

{ #category : #accessing }
EpRedo >> title [
	^ 'Apply changes'
]
