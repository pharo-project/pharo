"
I am deprecated. 
Now the radio buttons are created knowing their friends.

I am a group for radio buttons which ensures that only one button is activated at the same time.

I collaborate with
- RadioButtonPresenter are usually the buttons I manage.
- ValueHolder to link buttons.

I provide the following methods
- addRadioButton: to add the button in my collection and link it at the other.
- buttons getter of my collection.
- default: activate a button when it is built.

todo
- canDeselectByClick
"
Class {
	#name : #RadioButtonGroup,
	#superclass : #Object,
	#instVars : [
		'buttons',
		'currentlyActivated',
		'canDeselectByClick'
	],
	#category : #'Spec-Deprecated80-Support'
}

{ #category : #deprecation }
RadioButtonGroup class >> isDeprecated [ 

	^ true
]

{ #category : #api }
RadioButtonGroup >> addRadioButton: aButton [

	buttons do: [:b | 
		aButton whenActivatedDo: [ b privateSetState: false ].
		b whenActivatedDo: [ aButton privateSetState: false ]].
		
	aButton whenActivatedDo: [ self currentlyActivated: aButton ].
		
	buttons add: aButton.
	aButton privateSetState: false
]

{ #category : #accessing }
RadioButtonGroup >> buttons [
	^ buttons
]

{ #category : #api }
RadioButtonGroup >> canDeselectByClick [
	"Return true clicking on a selected radio button deselects it"

	^ canDeselectByClick value
]

{ #category : #api }
RadioButtonGroup >> canDeselectByClick: aBoolean [
	"Set if clicking on a selected radio button can deselect it"
	
	canDeselectByClick value: aBoolean
]

{ #category : #accessing }
RadioButtonGroup >> currentlyActivated [
	^ currentlyActivated
]

{ #category : #accessing }
RadioButtonGroup >> currentlyActivated: anObject [
	currentlyActivated := anObject
]

{ #category : #api }
RadioButtonGroup >> default: aButton [

	aButton privateSetState: true.
	aButton takeKeyboardFocus.
	aButton whenBuiltDo: [ aButton privateSetState: true ]
]

{ #category : #initialization }
RadioButtonGroup >> initialize [

	super initialize.

	buttons := OrderedCollection new.
	canDeselectByClick := false asValueHolder.
	currentlyActivated := nil asValueHolder
]

{ #category : #'as yet unclassified' }
RadioButtonGroup >> whenCanDeselectByClickChanged: aBlock [
	self deprecated: 'Use #whenCanDeselectByClickChangedDo: instead.' transformWith: '`@receiver whenCanDeselectByClickChanged: `@statements' -> '`@receiver whenCanDeselectByClickChangedDo: `@statements'.
	self whenCanDeselectByClickChangedDo: aBlock
]

{ #category : #'api-events' }
RadioButtonGroup >> whenCanDeselectByClickChangedDo: aBlock [
	"This method is triggered when `canDeselectByClick` changes"
	
	canDeselectByClick whenChangedDo: aBlock
]

{ #category : #'as yet unclassified' }
RadioButtonGroup >> whenCurrentlyActivatedChanged: aBlock [
	self deprecated: 'Use #whenCurrentlyActivatedChangedDo: instead.' transformWith: '`@receiver whenCurrentlyActivatedChanged: `@statements' -> '`@receiver whenCurrentlyActivatedChangedDo: `@statements'.
	self whenCurrentlyActivatedChangedDo: aBlock
]

{ #category : #'api-events' }
RadioButtonGroup >> whenCurrentlyActivatedChangedDo: aBlock [
	"This method is triggered when `canDeselectByClick` changes"
	
	currentlyActivated whenChangedDo: aBlock
]
