"
A base for button presenters, it defines basic functionality common to all buttons.

## Interesting API methods

- `SpAbstractButtonPresenter>>#icon`
- `SpAbstractButtonPresenter>>#icon:`
- `SpAbstractButtonPresenter>>#label`
- `SpAbstractButtonPresenter>>#label:`

## Events

- `SpAbstractButtonPresenter>>#whenIconChangedDo:`
- `SpAbstractButtonPresenter>>#whenLabelChangedDo:`

See also `SpButtonPresenter`, `SpMenuButtonPresenter`.
"
Class {
	#name : #SpAbstractButtonPresenter,
	#superclass : #SpAbstractWidgetPresenter,
	#instVars : [
		'#icon => SpObservableSlot',
		'#label => SpObservableSlot'
	],
	#category : #'Spec2-Core-Widgets'
}

{ #category : #private }
SpAbstractButtonPresenter >> addShortcutTipFor: aString [

	^ self 
		addShortcutTipFor: self shortcutCharacter
		on: aString
]

{ #category : #private }
SpAbstractButtonPresenter >> addShortcutTipFor: aCharacter on: aString [
	
	^ aString withAccentuatedCharacter: aCharacter
]

{ #category : #api }
SpAbstractButtonPresenter >> icon [
	"Answer the icon defined for this button (it can be nil)"
	^ icon
]

{ #category : #api }
SpAbstractButtonPresenter >> icon: anIcon [
	"Sets the icon to be displayed by the button. 
	 The icon must be a `Form`"

	icon := anIcon
]

{ #category : #api }
SpAbstractButtonPresenter >> iconName: aSymbol [

	self icon: (self iconNamed: aSymbol)
]

{ #category : #initialization }
SpAbstractButtonPresenter >> initialize [

	super initialize.
	
	label := ''.
	
	self whenLabelChangedDo: [ self changed: #label ].
	self whenIconChangedDo: [ self changed: #label ].

]

{ #category : #api }
SpAbstractButtonPresenter >> label [
	"Answers the label to be shown by the button"

	^ self addShortcutTipFor: label
]

{ #category : #api }
SpAbstractButtonPresenter >> label: aString [
	"Sets the label to be shown by the button."

	label := aString
]

{ #category : #localization }
SpAbstractButtonPresenter >> localeChanged [

	super localeChanged.
	self forceUpdateSlot: #label
	
]

{ #category : #private }
SpAbstractButtonPresenter >> shortcutCharacter [

	^ nil
]

{ #category : #'api-events' }
SpAbstractButtonPresenter >> whenIconChangedDo: aBlock [
	"Inform when an icon has been changed (invoking `SpAbstractButtonPresenter>>#icon:`). 
	 `aBlock` receives zero arguments."

	self property: #icon whenChangedDo: aBlock
]

{ #category : #'api-events' }
SpAbstractButtonPresenter >> whenLabelChangedDo: aBlock [
	"Inform when a label has been changed (invoking `SpAbstractButtonPresenter>>#label:`). 
	 `aBlock` receives zero arguments."

	self property: #label whenChangedDo: aBlock
]
