"
I implement behavior to apply and revert code changes. 

Important Note: My #entries are expected to be ordered as ""the oldest first"" (as in EpLog>>entries).
"
Class {
	#name : #EpLogBrowserOperationFactory,
	#superclass : #Object,
	#instVars : [
		'entries',
		'errorHandlerBlock',
		'logBrowserModel'
	],
	#category : #'EpiceaBrowsers-Integration'
}

{ #category : #public }
EpLogBrowserOperationFactory >> applyCodeChanges [
	"Note: This method is intended to be used from EpLogBrowser's preview mode, where the code changes were previously transformed at #newApplyPreviewLog."

	self
		trigger: [ 
			self entries do: [:each |
				self handleErrorDuring: [ 
					each content applyCodeChange ]]] 
		with: self newApplyEvent
]

{ #category : #public }
EpLogBrowserOperationFactory >> applyCompleteRefactoring [

	| refactoringEntry refactoring |
	refactoringEntry := self entries first. "Refactorings are applied one at time"
	refactoring := refactoringEntry content asRBRefactoring.
	
	self applyRBRefactoring: refactoring

]

{ #category : #public }
EpLogBrowserOperationFactory >> applyPropagateRefactoring [

	| refactoringEntry refactoring |
	refactoringEntry := self entries first. "Refactorings are applied one at time"
	refactoring := EpRBPropagateRefactoring target: refactoringEntry content asRBRefactoring.

	self applyRBRefactoring: refactoring

]

{ #category : #private }
EpLogBrowserOperationFactory >> applyRBRefactoring: aRBRefactoring [

	self 
		trigger: [
			self handleErrorDuring: [
				| aNautilusUI |
				aNautilusUI := NautilusUI on: Nautilus new.
				(NautilusRefactoring model: aNautilusUI) performRefactoring: aRBRefactoring ]]
		with: self newApplyEvent
]

{ #category : #accessing }
EpLogBrowserOperationFactory >> entries [
	^ entries
]

{ #category : #accessing }
EpLogBrowserOperationFactory >> entries: aCollection [
	"Important Note: My #entries must be sorted the oldest first."

	entries := aCollection
]

{ #category : #accessing }
EpLogBrowserOperationFactory >> errorHandlerBlock [
	^ errorHandlerBlock
]

{ #category : #accessing }
EpLogBrowserOperationFactory >> errorHandlerBlock: aBlock [
	errorHandlerBlock := aBlock
]

{ #category : #accessing }
EpLogBrowserOperationFactory >> events [

	^ self entries collect: [:each | each content ]
]

{ #category : #private }
EpLogBrowserOperationFactory >> handleErrorDuring: aBlock [
	"TODO: do not catch *all* Errors.
	This error handler exists as a workaround to skip any unexpected error when applying or reverting a code change from GUI.
	For example, when user reverts the protocol addition of a protocol that is already not present, and such action signals an unexpected error. The error in such case should be avoided via testing API."
	
	aBlock
		on: Error
		do: self errorHandlerBlock

]

{ #category : #private }
EpLogBrowserOperationFactory >> latestEventsFrom: previewEventsTheOldestFirst [
	"TODO: move method"
	
	| aLog filter |
	aLog := EpLog newWithStore: (OmMemoryStore withAll: previewEventsTheOldestFirst).
	logBrowserModel := EpLogBrowserModel newWithLog: aLog.
	filter := EpLatestCodeChangeFilter new
		logBrowserModel: logBrowserModel;
		yourself.
	^ aLog entries 
		select: [ :each | filter accepts: each ] 
		thenCollect: [ :each | each content ]
]

{ #category : #accessing }
EpLogBrowserOperationFactory >> logBrowserModel [
	^ logBrowserModel
]

{ #category : #accessing }
EpLogBrowserOperationFactory >> logBrowserModel: aBrowser [
	logBrowserModel := aBrowser
]

{ #category : #private }
EpLogBrowserOperationFactory >> newApplyEvent [

	^ EpRedo forAll: (self theLog referencesToAll: self entries)

]

{ #category : #public }
EpLogBrowserOperationFactory >> newApplyPreviewLog [

	| events |
	events := self latestEventsFrom: self events.

	events := events flatCollect: [:each | 
		each previewedApplyEvents ].
	
	^ EpLog newWithEvents: events

]

{ #category : #private }
EpLogBrowserOperationFactory >> newRevertEvent [

	^ EpUndo forAll: (self theLog referencesToAll: self entries)

]

{ #category : #public }
EpLogBrowserOperationFactory >> newRevertPreviewLog [

	| events |
	events := self latestEventsFrom: self events reversed.

	events := events flatCollect: [:each | 
		each asRevertedCodeChange previewedApplyEvents ].

	^ EpLog newWithEvents: events

]

{ #category : #public }
EpLogBrowserOperationFactory >> revertCodeChanges [

	self
		trigger: [ 
			self entries reverseDo: [:each | 
				self handleErrorDuring: [ 
					each content asRevertedCodeChange applyCodeChange ]]]
		with: self newRevertEvent
]

{ #category : #public }
EpLogBrowserOperationFactory >> revertCodeChangesInPreviewMode [
	"Note: This method is intended to be used from EpLogBrowser's preview mode, where the code changes were previously transformed at #newRevertPreviewLog. That's why here the transformed code changes are simply applied as if was a redo (apply) operation and not as an undo (revert)."

	self
		trigger: [ 
			self entries do: [:each |
				self handleErrorDuring: [ 
					each content applyCodeChange ]]] 
		with: self newRevertEvent
]

{ #category : #accessing }
EpLogBrowserOperationFactory >> theLog [

	^ logBrowserModel theLog
]

{ #category : #private }
EpLogBrowserOperationFactory >> trigger: aBlock with: anEpLogBrowserOperation [

	^ anEpLogBrowserOperation doInJob: aBlock
]
