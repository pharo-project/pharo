"
Toolbar to explore previous or next inpected elements
"
Class {
	#name : #EyeInspectorToolBar,
	#superclass : #ComposablePresenter,
	#instVars : [
		'previousButton',
		'nextButton',
		'inspectorChoice',
		'isUpdating'
	],
	#category : 'Spec-Inspector'
}

{ #category : #specs }
EyeInspectorToolBar class >> defaultSpec [
	<spec>
	^ SpecLayout composed
		newRow: [ :r | 
			r
				add: #previousButton width: 24;
				add: #nextButton width: 28;
				add: #inspectorChoice
		];
	yourself.
]

{ #category : #accessing }
EyeInspectorToolBar >> history [
	^ self inspectorWrapper history
]

{ #category : #initialization }
EyeInspectorToolBar >> initialize [
	isUpdating := false.
	super initialize.
]

{ #category : #initialization }
EyeInspectorToolBar >> initializeWidgets [
	" nothing to do"
]

{ #category : #accessing }
EyeInspectorToolBar >> inspectorChoice [
	inspectorChoice
		ifNil: [ 
			inspectorChoice := self instantiate: DropListPresenter.
			inspectorChoice
				whenSelectedItemChanged: [ :inspectorType |
					self preventUpdatesDuring: [
						self inspectorType: inspectorType ]]].
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
	nextButton
		ifNil: [ 
			nextButton := self instantiate: ButtonPresenter.
			nextButton 
				label: '>';
				action: [ 
					self inspectorWrapper inspectNext.
					self update. ]].
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
	previousButton
		ifNil: [ 
			previousButton := self instantiate: ButtonPresenter.
			previousButton label: '<'.
			previousButton
				action: [ 
					self inspectorWrapper inspectPrevious.
					self update. ] ].
	^ previousButton
]

{ #category : #event-handling }
EyeInspectorToolBar >> update [
	self preventUpdatesDuring: [ 
		self updateInspectorChoice.
		self updateInspectorType.
		self previousButton enabled: self history hasPrevious.
		self nextButton enabled: self history hasNext ]
]

{ #category : #event-handling }
EyeInspectorToolBar >> updateInspectorChoice [
	self inspectorTypes
		ifNotNil: [ :variants | 
			self inspectorChoice listItems = variants
				ifTrue: [  ^ self ].
			self inspectorChoice
				items: variants;
				displayBlock: [ :label | label ]]
]

{ #category : #event-handling }
EyeInspectorToolBar >> updateInspectorType [
	
	
	self inspectorType
		ifNotNil: [ :inspectorClass | 
			self inspectorChoice selectedItem = inspectorClass
				ifTrue: [  ^ self ].
			self inspectorChoice setSelectedItem: inspectorClass ].
]
