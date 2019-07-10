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
	#name : #SpRadioButtonGroup,
	#superclass : #Object,
	#instVars : [
		'buttons',
		'currentlyActivated',
		'canDeselectByClick'
	],
	#category : #'Spec2-Deprecated-Support'
}

{ #category : #deprecation }
SpRadioButtonGroup class >> isDeprecated [ 

	^ true
]

{ #category : #api }
SpRadioButtonGroup >> addRadioButton: aButton [

	buttons do: [:b | 
		aButton whenActivatedDo: [ b privateSetState: false ].
		b whenActivatedDo: [ aButton privateSetState: false ]].
		
	aButton whenActivatedDo: [ self currentlyActivated: aButton ].
		
	buttons add: aButton.
	aButton privateSetState: false
]

{ #category : #accessing }
SpRadioButtonGroup >> buttons [
	^ buttons
]

{ #category : #api }
SpRadioButtonGroup >> canDeselectByClick [
	"Return true clicking on a selected radio button deselects it"

	^ canDeselectByClick value
]

{ #category : #api }
SpRadioButtonGroup >> canDeselectByClick: aBoolean [
	"Set if clicking on a selected radio button can deselect it"
	
	canDeselectByClick value: aBoolean
]

{ #category : #accessing }
SpRadioButtonGroup >> currentlyActivated [
	^ currentlyActivated
]

{ #category : #accessing }
SpRadioButtonGroup >> currentlyActivated: anObject [
	currentlyActivated := anObject
]

{ #category : #api }
SpRadioButtonGroup >> default: aButton [

	aButton privateSetState: true.
	aButton takeKeyboardFocus.
	aButton whenBuiltDo: [ aButton privateSetState: true ]
]

{ #category : #initialization }
SpRadioButtonGroup >> initialize [

	super initialize.

	buttons := OrderedCollection new.
	canDeselectByClick := false asValueHolder.
	currentlyActivated := nil asValueHolder
]

{ #category : #'as yet unclassified' }
SpRadioButtonGroup >> whenCanDeselectByClickChanged: aBlock [
	self deprecated: 'Use #whenCanDeselectByClickChangedDo: instead.' transformWith: '`@receiver whenCanDeselectByClickChanged: `@statements' -> '`@receiver whenCanDeselectByClickChangedDo: `@statements'.
	self whenCanDeselectByClickChangedDo: aBlock
]

{ #category : #'api-events' }
SpRadioButtonGroup >> whenCanDeselectByClickChangedDo: aBlock [
	"This method is triggered when `canDeselectByClick` changes"
	
	canDeselectByClick whenChangedDo: aBlock
]

{ #category : #'as yet unclassified' }
SpRadioButtonGroup >> whenCurrentlyActivatedChanged: aBlock [
	self deprecated: 'Use #whenCurrentlyActivatedChangedDo: instead.' transformWith: '`@receiver whenCurrentlyActivatedChanged: `@statements' -> '`@receiver whenCurrentlyActivatedChangedDo: `@statements'.
	self whenCurrentlyActivatedChangedDo: aBlock
]

{ #category : #'api-events' }
SpRadioButtonGroup >> whenCurrentlyActivatedChangedDo: aBlock [
	"This method is triggered when `canDeselectByClick` changes"
	
	currentlyActivated whenChangedDo: aBlock
]
