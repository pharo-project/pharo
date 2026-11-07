Class {
	#name : 'StControlledRenameClassPreviewPresenter',
	#superclass : 'StRenameClassPreviewPresenter',
	#instVars : [ 'work', 'publications' ],
	#category : 'Refactoring-UI-Tests-UI',
	#package : 'Refactoring-UI-Tests',
	#tag : 'UI'
}

{ #category : 'testing' }
StControlledRenameClassPreviewPresenter >> schedulePreview: aBlock [

	work ifNil: [ work := OrderedCollection new ].
	work add: aBlock
]

{ #category : 'testing' }
StControlledRenameClassPreviewPresenter >> dispatchPreview: aBlock [

	publications ifNil: [ publications := OrderedCollection new ].
	publications add: aBlock
]

{ #category : 'testing' }
StControlledRenameClassPreviewPresenter >> computeNextPreview [

	work removeFirst value
]

{ #category : 'testing' }
StControlledRenameClassPreviewPresenter >> publishNextPreview [

	publications removeFirst value
]

{ #category : 'testing' }
StControlledRenameClassPreviewPresenter >> currentRequest [

	^ previewRequest
]

{ #category : 'testing' }
StControlledRenameClassPreviewPresenter >> performChanges [

	self error: 'Pending preview must never be applied'
]

{ #category : 'testing' }
StControlledRenameClassPreviewPresenter >> queuedComputations [

	^ work ifNil: [ 0 ] ifNotNil: [ work size ]
]
