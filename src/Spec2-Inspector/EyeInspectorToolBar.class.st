"
Toolbar to explore previous or next inpected elements
"
Class {
	#name : #EyeInspectorToolBar,
	#superclass : #SpPresenter,
	#instVars : [
		'previousButton',
		'nextButton',
		'inspectorChoice',
		'isUpdating'
	],
	#category : #'Spec2-Inspector-Widgets'
}

{ #category : #specs }
EyeInspectorToolBar class >> defaultSpec [
	<spec>
	^ SpBoxLayout newHorizontal
		add: #previousButton
			withConstraints: [ :constraints | 
			constraints
				width: 24;
				padding: 2 ];
		add: #nextButton
			withConstraints: [ :constraints | 
			constraints
				width: 24;
				padding: 2 ];
		add: #inspectorChoice;
		yourself
]

{ #category : #accessing }
EyeInspectorToolBar >> history [
	^ self inspectorWrapper history
]

{ #category : #initialization }
EyeInspectorToolBar >> initialize [
	super initialize.
	isUpdating := false
]

{ #category : #initialization }
EyeInspectorToolBar >> initializePresenter [
	inspectorChoice whenSelectedItemChanged: [ :inspectorType | self preventUpdatesDuring: [ self inspectorType: inspectorType ] ]
]

{ #category : #initialization }
EyeInspectorToolBar >> initializeWidgets [
	inspectorChoice := self newDropList.
	previousButton := self newButton.
	nextButton := self newButton.

	previousButton
		label: '<';
		action: [ self inspectorWrapper inspectPrevious.
			self update ].
	nextButton
		label: '>';
		action: [ self inspectorWrapper inspectNext.
			self update ].
	inspectorChoice displayBlock: [ :label | label ]
]

{ #category : #accessing }
EyeInspectorToolBar >> inspectorChoice [
	^ inspectorChoice
]

{ #category : #accessing }
EyeInspectorToolBar >> inspectorType [
	^ self inspectorWrapper inspectorType
]

{ #category : #actions }
EyeInspectorToolBar >> inspectorType: anInspectorClass [
	^ self inspectorWrapper inspectorType: anInspectorClass
]

{ #category : #accessing }
EyeInspectorToolBar >> inspectorTypes [
	^ self inspectorWrapper inspectorTypes
]

{ #category : #accessing }
EyeInspectorToolBar >> inspectorWrapper [
	^ self owner
]

{ #category : #accessing }
EyeInspectorToolBar >> nextButton [
	^ nextButton
]

{ #category : #private }
EyeInspectorToolBar >> preventUpdatesDuring: aBlock [
	"Used to prevent back-firing when the toolbar is update from the outside"
	isUpdating ifTrue: [ 
		^ self ].
	isUpdating := true.
	aBlock ensure: [ isUpdating := false ]
]

{ #category : #accessing }
EyeInspectorToolBar >> previousButton [
	^ previousButton
]

{ #category : #'event-handling' }
EyeInspectorToolBar >> update [
	self
		preventUpdatesDuring: [ self updateInspectorChoice.
			self updateInspectorType.
			previousButton enabled: self history hasPrevious.
			nextButton enabled: self history hasNext ]
]

{ #category : #'event-handling' }
EyeInspectorToolBar >> updateInspectorChoice [
	self inspectorTypes
		ifNotNil: [ :variants | 
			inspectorChoice listItems = variants ifTrue: [ ^ self ].
			inspectorChoice items: variants ]
]

{ #category : #'event-handling' }
EyeInspectorToolBar >> updateInspectorType [
	self inspectorType
		ifNotNil: [ :inspectorClass | 
			inspectorChoice selectedItem = inspectorClass ifTrue: [ ^ self ].
			inspectorChoice selectedItem: inspectorClass ]
]
