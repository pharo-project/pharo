"
A button to be shown in a `SpToolbarPresenter`.
A `SpToolbarButtonPresenter` can show a badge.
"
Class {
	#name : #SpToolbarButtonPresenter,
	#superclass : #SpAbstractWidgetPresenter,
	#instVars : [
		'#label => SpObservableSlot',
		'#icon => SpObservableSlot',
		'#action',
		'#badge'
	],
	#category : #'Spec2-Core-Widgets-Toolbar'
}

{ #category : #specs }
SpToolbarButtonPresenter class >> adapterName [

	^ #ToolbarButtonAdapter
]

{ #category : #documentation }
SpToolbarButtonPresenter class >> documentFactoryMethodSelector [

	^ #newToolbarButton
]

{ #category : #api }
SpToolbarButtonPresenter >> action [
	"Answer the block performed when the button is clicked."

	^ action
]

{ #category : #api }
SpToolbarButtonPresenter >> action: aBlock [
	"Set the block performed when the button is clicked. 
	 `aBlock` receives zero arguments."

	action := aBlock
]

{ #category : #api }
SpToolbarButtonPresenter >> badge [
	"Answer the badge that will be shown with the button."

	^ badge
]

{ #category : #api }
SpToolbarButtonPresenter >> badge: aString [
	"Set the badge to be shown with the button.
	 A badge is an extra piece of information that can be added to the button. Typically, this 
	 is used to show some status (for example, number of elements that will be touched if 
	 the button is pressed)"
	
	badge := aString
]

{ #category : #simulating }
SpToolbarButtonPresenter >> click [
	
	self action cull: self
]

{ #category : #execution }
SpToolbarButtonPresenter >> execute [

	self withAdapterDo: [ :anAdapter | anAdapter execute ]
]

{ #category : #api }
SpToolbarButtonPresenter >> icon [
	"Answer the icon (an instance of `Form`) defined for this button (it can be nil)"

	^ icon
]

{ #category : #api }
SpToolbarButtonPresenter >> icon: anObject [
	icon := anObject
]

{ #category : #initialization }
SpToolbarButtonPresenter >> initialize [
	super initialize.
]

{ #category : #api }
SpToolbarButtonPresenter >> label [
	"Answer the label to be shown by the button"

	^ label
]

{ #category : #api }
SpToolbarButtonPresenter >> label: aString [
	"Set the label to be shown by the button."

	label := aString
]

{ #category : #'api-events' }
SpToolbarButtonPresenter >> whenIconChangedDo: aBlock [ 
	"Inform when icon property has changed. 
	 `aBlock` has three optional arguments: 
	 - new value
	 - old value
	 - the announcement triggering this action"

	self property: #icon whenChangedDo: aBlock 
]

{ #category : #'api-events' }
SpToolbarButtonPresenter >> whenLabelChangedDo: aBlock [ 
	"Inform when label property has changed. 
	 `aBlock` has three optional arguments: 
	 - new value
	 - old value
	 - the announcement triggering this action"

	self property: #label whenChangedDo: aBlock 
]
