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
	#category : #'Spec-Core-Widgets'
}

{ #category : #specs }
RadioButtonGroupPresenter class >> defaultSpec [
	<spec: #default>
	^ SpecLayout composed
		newColumn: [ :column |  ];
		yourself
]

{ #category : #example }
RadioButtonGroupPresenter class >> example [
	| example |
	example := RadioButtonGroupPresenter new.
	example
		addRadioButton: (RadioButtonPresenter new label: '1');
		addRadioButton: (RadioButtonPresenter new label: '2');
		addRadioButton: (RadioButtonPresenter new label: '3');
		whenCurrentActivatedChanged: [ self inform: example currentActivated label ].
	example openWithSpec.
	^ example
]

{ #category : #protocol }
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

{ #category : #protocol }
RadioButtonGroupPresenter >> canDeselectByClick [
	<api: #inspect>
	"Return true clicking on a selected radio button deselects it"

	^ canDeselectByClick value
]

{ #category : #protocol }
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

{ #category : #protocol }
RadioButtonGroupPresenter >> default: aButton [
	aButton privateSetState: true.
	aButton takeKeyboardFocus.
	aButton
		whenBuiltDo: [ aButton privateSetState: true.
			self currentActivated: aButton]
	
]

{ #category : #protocol }
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
		whenCanDeselectByClickChanged: [ :aBoolean | buttons do: [ :each | each canDeselectByClick: aBoolean ] ]
]

{ #category : #protocol }
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

{ #category : #'protocol-events' }
RadioButtonGroupPresenter >> whenCanDeselectByClickChanged: aBlock [
	"This method is triggered when `canDeselectByClick` changes"
	
	canDeselectByClick whenChangedDo: aBlock
]

{ #category : #'protocol-events' }
RadioButtonGroupPresenter >> whenCurrentActivatedChanged: aBlock [

	currentActivated whenChangedDo: aBlock
]
