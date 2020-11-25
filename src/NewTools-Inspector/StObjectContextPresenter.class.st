"
A presenter object inspection that is aware of the inspection context.
It is used to build enhance inspection view (e.g. show or not the evaluator).
"
Class {
	#name : #StObjectContextPresenter,
	#superclass : #StPresenter,
	#instVars : [
		'view',
		'evaluator',
		'model',
		'activationBlock'
	],
	#category : #'NewTools-Inspector-View'
}

{ #category : #api }
StObjectContextPresenter >> activateOnDoubleClick [
	
	view hasOutputActivationPort ifFalse: [ ^ self ].
	view activateOnDoubleClick
]

{ #category : #api }
StObjectContextPresenter >> activateOnSingleClick [
	
	view hasOutputActivationPort ifFalse: [ ^ self ].
	view activateOnSingleClick
]

{ #category : #commands }
StObjectContextPresenter >> doEvaluateAndGo [
	| value |
	
	activationBlock ifNil: [ ^ self ].
	
	value := evaluator 
		evaluate: evaluator selectedTextOrLine
		ifError: [ :e | 
			evaluator insertErrorPopover: (SpCodePopoverErrorPresenter 
				newCode: evaluator 
				message: e messageText) 
		atIndex: evaluator selectionInterval end - 1 ].
	
	activationBlock value: (StEvaluatedSelectionModel newValue: value)
]

{ #category : #private }
StObjectContextPresenter >> evaluatorActions [
	
	^ CmCommandGroup forSpec
		beDisplayedAsGroup;
		register: (StEvaluateCommand forSpecContext: self);
		yourself
]

{ #category : #private }
StObjectContextPresenter >> evaluatorInitialText [

	^ '"{1}"
{2}' 
	format: { 
		self model smallDescriptionString.
		self model codeSnippet }
]

{ #category : #testing }
StObjectContextPresenter >> hasEvaluator [

	^ self model hasEvaluator
]

{ #category : #testing }
StObjectContextPresenter >> hasOutputActivationPort [
	
	^ self hasEvaluator or: [ view hasOutputActivationPort ]
]

{ #category : #initialization }
StObjectContextPresenter >> initializePresenters [

	view := self model newInspectionView.
	self hasEvaluator
		ifTrue: [ self intializePresentersWithEvaluator ]
		ifFalse: [ self intializePresentersWithoutEvaluator ]
]

{ #category : #initialization }
StObjectContextPresenter >> intializePresentersWithEvaluator [
	
	evaluator := self newCode
		beForObject: self model inspectedObject;
		text: self evaluatorInitialText;
		contextMenu: [ self evaluatorActions asMenuPresenter ];
		contextKeyBindings: self evaluatorActions asKMCategory;
		whenTextChangedDo: [ :aString | self model codeSnippet: aString ];
		yourself.
	
	self layout: (SpPanedLayout newVertical
		position: 70 percent;
		add: view;
		add: evaluator;
		yourself)
]

{ #category : #initialization }
StObjectContextPresenter >> intializePresentersWithoutEvaluator [
	
	self layout: (SpBoxLayout newVertical
		add: view;
		yourself)
]

{ #category : #'accessing model' }
StObjectContextPresenter >> model [

	^ model
]

{ #category : #transmission }
StObjectContextPresenter >> outputActivationPort [

	^ view hasOutputActivationPort 
		ifTrue: [ (SpActivationPort newPresenter: self), view outputActivationPort ]
		ifFalse: [ SpActivationPort newPresenter: self ]
]

{ #category : #'accessing model' }
StObjectContextPresenter >> setModelBeforeInitialization: aModel [
	
	model := aModel
]

{ #category : #'api-events' }
StObjectContextPresenter >> whenActivatedDo: aBlock [

	activationBlock := aBlock
]
