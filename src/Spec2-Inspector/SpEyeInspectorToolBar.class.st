"
Toolbar to explore previous or next inpected elements
"
Class {
	#name : #SpEyeInspectorToolBar,
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
SpEyeInspectorToolBar class >> defaultSpec [
	<spec>
	^ SpLayout composed
		newRow: [ :r | 
			r
				add: #previousButton width: 24;
				add: #nextButton width: 28;
				add: #inspectorChoice
		];
	yourself.
]

{ #category : #accessing }
SpEyeInspectorToolBar >> history [
	^ self inspectorWrapper history
]

{ #category : #initialization }
SpEyeInspectorToolBar >> initialize [
	isUpdating := false.
	super initialize.
]

{ #category : #initialization }
SpEyeInspectorToolBar >> initializeWidgets [
	" nothing to do"
]

{ #category : #accessing }
SpEyeInspectorToolBar >> inspectorChoice [
	inspectorChoice
		ifNil: [ 
			inspectorChoice := self instantiate: SpDropListPresenter.
			inspectorChoice
				whenSelectedItemChanged: [ :inspectorType |
					self preventUpdatesDuring: [
						self inspectorType: inspectorType ]]].
	^ inspectorChoice
]

{ #category : #accessing }
SpEyeInspectorToolBar >> inspectorType [
	^ self inspectorWrapper inspectorType
]

{ #category : #actions }
SpEyeInspectorToolBar >> inspectorType: anInspectorClass [
	^ self inspectorWrapper inspectorType: anInspectorClass
]

{ #category : #accessing }
SpEyeInspectorToolBar >> inspectorTypes [
	^ self inspectorWrapper inspectorTypes
]

{ #category : #accessing }
SpEyeInspectorToolBar >> inspectorWrapper [
	^ self owner
]

{ #category : #accessing }
SpEyeInspectorToolBar >> nextButton [
	nextButton
		ifNil: [ 
			nextButton := self instantiate: SpButtonPresenter.
			nextButton 
				label: '>';
				action: [ 
					self inspectorWrapper inspectNext.
					self update. ]].
	^ nextButton
]

{ #category : #private }
SpEyeInspectorToolBar >> preventUpdatesDuring: aBlock [
	"Used to prevent back-firing when the toolbar is update from the outside"
	isUpdating ifTrue: [ 
		^ self ].
	isUpdating := true.
	aBlock ensure: [ isUpdating := false ]
]

{ #category : #accessing }
SpEyeInspectorToolBar >> previousButton [
	previousButton
		ifNil: [ 
			previousButton := self instantiate: SpButtonPresenter.
			previousButton label: '<'.
			previousButton
				action: [ 
					self inspectorWrapper inspectPrevious.
					self update. ] ].
	^ previousButton
]

{ #category : #'event-handling' }
SpEyeInspectorToolBar >> update [
	self preventUpdatesDuring: [ 
		self updateInspectorChoice.
		self updateInspectorType.
		self previousButton enabled: self history hasPrevious.
		self nextButton enabled: self history hasNext ]
]

{ #category : #'event-handling' }
SpEyeInspectorToolBar >> updateInspectorChoice [
	self inspectorTypes
		ifNotNil: [ :variants | 
			self inspectorChoice listItems = variants
				ifTrue: [  ^ self ].
			self inspectorChoice
				items: variants;
				displayBlock: [ :label | label ]]
]

{ #category : #'event-handling' }
SpEyeInspectorToolBar >> updateInspectorType [
	
	
	self inspectorType
		ifNotNil: [ :inspectorClass | 
			self inspectorChoice selectedItem = inspectorClass
				ifTrue: [  ^ self ].
			self inspectorChoice setSelectedItem: inspectorClass ].
]
