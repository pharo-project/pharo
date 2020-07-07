"
I'm a LabelPresenter, made to add labels to spec components.

! SpLabelPresenter 
A label presenter displays smalls (or medium) amounts of text.

Examples:
> self example

NOTE: In the future, it should be possible to attach another widget to a label, but for now it is not implementes.

@@TODO

!! Transmissions
- #inputLabelPort (default)
"
Class {
	#name : #SpLabelPresenter,
	#superclass : #SpAbstractWidgetPresenter,
	#instVars : [
		'#label => SpObservableSlot'
	],
	#category : #'Spec2-Core-Widgets'
}

{ #category : #specs }
SpLabelPresenter class >> adapterName [

	^ #LabelAdapter
]

{ #category : #example }
SpLabelPresenter class >> example [
	"This example opens a spec window with a label."

	^ SpLabelPresenter new
		label: 'Label example';
		openWithSpec.
]

{ #category : #specs }
SpLabelPresenter class >> title [

	^ 'Label presenter'
]

{ #category : #testing }
SpLabelPresenter >> canTakeKeyboardFocus [

	^ false
]

{ #category : #initialization }
SpLabelPresenter >> defaultColor [

	self flag: #TOREMOVE. "This needs to be removed"
	^ self theme textColor
]

{ #category : #initialization }
SpLabelPresenter >> initialize [
	super initialize.

	label := ''.
	self whenLabelChangedDo: [ self changed: #getText ]
]

{ #category : #api }
SpLabelPresenter >> label [
	^ label
]

{ #category : #api }
SpLabelPresenter >> label: aText [
	"Set the text of the label"

	label := aText
]

{ #category : #localization }
SpLabelPresenter >> localeChanged [

	super localeChanged.
	self forceUpdateSlot: #label
		
]

{ #category : #'api-events' }
SpLabelPresenter >> whenLabelChangedDo: aValuable [
	self property: #label whenChangedDo: aValuable
]
