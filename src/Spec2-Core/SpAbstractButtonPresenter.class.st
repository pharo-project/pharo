"
A base for button presenters, it defines basic functionality common to all buttons.
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
	"Answer the icon (an instance of `Form`) defined for this button (it can be nil)"

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
	"Answer the label to be shown by the button"

	^ self addShortcutTipFor: label
]

{ #category : #api }
SpAbstractButtonPresenter >> label: aString [
	"Set the label to be shown by the button."

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
	"Inform when icon property has changed. 
	 `aBlock` has three optional arguments: 
	 - new value
	 - old value
	 - the announcement triggering this action"

	self property: #icon whenChangedDo: aBlock
]

{ #category : #'api-events' }
SpAbstractButtonPresenter >> whenLabelChangedDo: aBlock [
	"Inform when label property has changed. 
	 `aBlock` has three optional arguments: 
	 - new value
	 - old value
	 - the announcement triggering this action"

	self property: #label whenChangedDo: aBlock
]
