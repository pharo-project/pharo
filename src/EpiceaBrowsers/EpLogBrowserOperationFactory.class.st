"
I implement behavior to apply and revert code changes.
"
Class {
	#name : #EpLogBrowserOperationFactory,
	#superclass : #Object,
	#instVars : [
		'entries',
		'errorHandlerBlock',
		'logBrowserModel'
	],
	#category : 'EpiceaBrowsers-Integration'
}

{ #category : #public }
EpLogBrowserOperationFactory >> applyCodeChanges [

	self
		trigger: [ 
			self entries reverseDo: [:each |
				self handleErrorDuring: [ 
					each content applyCodeChange ]]] 
		with: self newApplyEvent
]

{ #category : #public }
EpLogBrowserOperationFactory >> applyCompleteRefactoring [

	| refactoringEntry refactoring |
	refactoringEntry := self entries first entries first. "TODO: only first?"
	refactoring := refactoringEntry content asRBRefactoring.
	
	self applyRBRefactoring: refactoring

]

{ #category : #public }
EpLogBrowserOperationFactory >> applyPropagateRefactoring [

	| refactoringEntry refactoring |
	refactoringEntry := self entries first entries first. "TODO: only first?"
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

{ #category : #private }
EpLogBrowserOperationFactory >> handleErrorDuring: aBlock [
	"TODO: do not catch *all* Errors.
	This error handler exists as a workaround to skip any unexpected error when applying or reverting a code change from GUI.
	For example, when user reverts the protocol addition of a protocol that is already not present, and such action signals an unexpected error. The error in such case should be avoided via testing API."
	
	aBlock
		on: Error
		do: self errorHandlerBlock

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
	events := self entries flatCollect: [:each | 
		each content previewedApplyEvents ].
	
	^ self newPreviewLogFor: events

]

{ #category : #private }
EpLogBrowserOperationFactory >> newPreviewLogFor: rawEvents [

	| aLog filter latestEvents |
	self flag: #fix.
	aLog := EpLog newWithStore: (OmMemoryStore withAll: rawEvents).
	logBrowserModel := EpLogBrowserModel newWithLog: aLog.
	filter := EpLatestCodeChangeFilter new logBrowserModel: logBrowserModel; yourself.
	latestEvents := aLog entries 
		select: [:each | filter accepts: each ]
		thenCollect: [:each | each content ].

	^ EpLog newWithStore: (OmMemoryStore withAll: latestEvents)

]

{ #category : #private }
EpLogBrowserOperationFactory >> newRevertEvent [

	^ EpUndo forAll: (self theLog referencesToAll: self entries)

]

{ #category : #public }
EpLogBrowserOperationFactory >> newRevertPreviewLog [

	| events |
	events := self entries reversed flatCollect: [:each | 
		each content asRevertedCodeChange previewedApplyEvents ].

	^ self newPreviewLogFor: events

]

{ #category : #public }
EpLogBrowserOperationFactory >> revertCodeChanges [

	self
		trigger: [ 
			self entries do: [:each | 
				self handleErrorDuring: [ 
					each content asRevertedCodeChange applyCodeChange ]]]
		with: self newRevertEvent
]

{ #category : #public }
EpLogBrowserOperationFactory >> revertCodeChangesInPreviewMode [

	self
		trigger: [ 
			self entries reverseDo: [:each |
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
