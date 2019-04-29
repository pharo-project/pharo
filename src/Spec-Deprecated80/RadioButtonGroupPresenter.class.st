"
I am a group for radio buttons.
See DynamicComposablePresenter

self example

You can also have a look at MrAndMrsRadioButton and RadioButtonGroupExample for more examples.

I ensures that only one button is activated at the same time and I hold the button activated.

I provide the following variables and their accessors
- buttons is the collection of the buttons I manage.
- currentActivated is the last activated button.

I provide the following methods
- addRadioButton: to add a button in my collection and link it at others.
- default: to activate the button when I am built.
- linkRadioButton: to make the button deselected when another is selected.

defaultOnce: is use to avoid a problem appearing with default: when it is include in other DynamicComposablePresenter but it should be replace by a fix.

I use the following method for my internal work
- createWidgets assign each button of my collection in my super class collection that manage dynamical subwidgets.
- dynamicLayout refresh the layout using my super class collection.
- rebuildWidget trigger the steps for refresh the widget and the layout.

todo
- canDeselectByClick
"
Class {
	#name : #RadioButtonGroupPresenter,
	#superclass : #DynamicComposablePresenter,
	#instVars : [
		'buttons',
		'canDeselectByClick',
		'currentActivated'
	],
	#category : #'Spec-Deprecated80'
}

{ #category : #specs }
RadioButtonGroupPresenter class >> defaultSpec [
	<spec: #default>
	^ SpecLayout composed
		newColumn: [ :column |  ];
		yourself
]

{ #category : #'as yet unclassified' }
RadioButtonGroupPresenter class >> example [
	<sampleInstance>
	| example |
	example := RadioButtonGroupPresenter new.
	example
		addRadioButton: (RadioButtonPresenter new label: '1');
		addRadioButton: (RadioButtonPresenter new label: '2');
		addRadioButton: (RadioButtonPresenter new label: '3');
		whenCurrentActivatedChangedDo: [ self inform: example currentActivated label ].
	example openWithSpec.
	^ example
]

{ #category : #specs }
RadioButtonGroupPresenter class >> isDeprecated [

	^ true
]

{ #category : #api }
RadioButtonGroupPresenter >> addRadioButton: aButton [
	self linkRadioButton: aButton.
	aButton canDeselectByClick: self canDeselectByClick.
	buttons add: aButton.
	aButton privateSetState: false;
		owner: self.
	self focusOrder add: aButton.
	self rebuildWidget
]

{ #category : #accessing }
RadioButtonGroupPresenter >> buttons [
	^ buttons
]

{ #category : #api }
RadioButtonGroupPresenter >> canDeselectByClick [
	<api: #inspect>
	"Return true clicking on a selected radio button deselects it"

	^ canDeselectByClick value
]

{ #category : #api }
RadioButtonGroupPresenter >> canDeselectByClick: aBoolean [
	<api: #boolean getter: #canDeselectByClick registration: #whenCanDeselectByClickChanged>
	"Set if clicking on a selected radio button can deselect it"
	
	canDeselectByClick value: aBoolean
]

{ #category : #accessing }
RadioButtonGroupPresenter >> createWidgets [
	buttons
		withIndexDo: [ :each :i | 
			self assign: each to: ('radio' , i asString) asSymbol.
			 ]
]

{ #category : #accessing }
RadioButtonGroupPresenter >> currentActivated [
	^ currentActivated value
]

{ #category : #accessing }
RadioButtonGroupPresenter >> currentActivated: aRadioButton [
	currentActivated value: aRadioButton
]

{ #category : #api }
RadioButtonGroupPresenter >> default: aButton [
	aButton privateSetState: true.
	aButton takeKeyboardFocus.
	aButton
		whenBuiltDo: [ aButton privateSetState: true.
			self currentActivated: aButton]
	
]

{ #category : #api }
RadioButtonGroupPresenter >> defaultOnce: aButton [
	aButton privateSetState: true.
	self currentActivated: aButton.
	"This method is used to avoid a problem of default: with a DynamicComposablePresenter but a fix would be better (if possible)"
]

{ #category : #specs }
RadioButtonGroupPresenter >> dynamicLayout [
	|lay|
	lay := SpecLayout composed
		newColumn: [ :column | 
			widgets keysDo: [ :item | column newRow: [ :row | row add: item ] ].
			column newRow: [ :row |  ] ].
	self layout: lay.
	^ lay
]

{ #category : #initialization }
RadioButtonGroupPresenter >> initialize [
	buttons := OrderedCollection new.
	canDeselectByClick := false asValueHolder.
	currentActivated := nil asValueHolder.
	super initialize.
]

{ #category : #initialization }
RadioButtonGroupPresenter >> initializePresenter [
	self
		whenCanDeselectByClickChangedDo:
			[ :aBoolean | buttons do: [ :each | each canDeselectByClick: aBoolean ] ]
]

{ #category : #api }
RadioButtonGroupPresenter >> linkRadioButton: aButton [
	buttons
		do: [ :b | 
			aButton whenActivatedDo: [ b privateSetState: false ].
			b whenActivatedDo: [ aButton privateSetState: false ] ].
	aButton whenActivatedDo: [ self currentActivated: aButton ].
]

{ #category : #accessing }
RadioButtonGroupPresenter >> rebuildWidget [
	self widgets removeAll.
	self createWidgets.
	self needFullRebuild: false.
	self buildWithSpecLayout: self dynamicLayout
]

{ #category : #'as yet unclassified' }
RadioButtonGroupPresenter >> whenCanDeselectByClickChanged: aBlock [
	self deprecated: 'Use #whenCanDeselectByClickChangedDo: instead.' transformWith: '`@receiver whenCanDeselectByClickChanged: `@statements' -> '`@receiver whenCanDeselectByClickChangedDo: `@statements'.
	self whenCanDeselectByClickChangedDo: aBlock
]

{ #category : #'api-events' }
RadioButtonGroupPresenter >> whenCanDeselectByClickChangedDo: aBlock [
	"This method is triggered when `canDeselectByClick` changes"
	
	canDeselectByClick whenChangedDo: aBlock
]

{ #category : #'as yet unclassified' }
RadioButtonGroupPresenter >> whenCurrentActivatedChanged: aBlock [
	self deprecated: 'Use #whenCurrentActivatedChangedDo: instead.' transformWith: '`@receiver whenCurrentActivatedChanged: `@statements' -> '`@receiver whenCurrentActivatedChangedDo: `@statements'.
	self whenCurrentActivatedChangedDo: aBlock
]

{ #category : #'api-events' }
RadioButtonGroupPresenter >> whenCurrentActivatedChangedDo: aBlock [

	currentActivated whenChangedDo: aBlock
]
