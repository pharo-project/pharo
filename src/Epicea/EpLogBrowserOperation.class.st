"
I represent an operation done in a EpLogBrowser.
"
Class {
	#name : #EpLogBrowserOperation,
	#superclass : #EpEvent,
	#instVars : [
		'entryReferences'
	],
	#category : 'Epicea-Model'
}

{ #category : #triggering }
EpLogBrowserOperation >> asEpiceaEvent [

	^ self
]

{ #category : #copying }
EpLogBrowserOperation >> copyWithEntryReferences: aCollectionOfEntryReferences [
	^self subclassResponsibility
]

{ #category : #triggering }
EpLogBrowserOperation >> doInJob: aBlock [
	"Evaluate the block. This is a workaround to make Job consider me as the Job's owner."

	^ [ aBlock value ] asJob
		title: '';
		run
]

{ #category : #accessing }
EpLogBrowserOperation >> entryReferences [
	^ entryReferences
]

{ #category : #initializing }
EpLogBrowserOperation >> initializeWith: aCollectionOfEntryReferences [
	self initialize.
	entryReferences := aCollectionOfEntryReferences
]

{ #category : #triggering }
EpLogBrowserOperation >> isEpiceaInterestingJobOwner [

	^ true
]
