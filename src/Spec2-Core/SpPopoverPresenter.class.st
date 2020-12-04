"
A presenter to display a popover. 
`SpPopoverPresenter` will be relative to its owner, but Iitcan be shown pointing to a particular place (see `SpPopoverPresenter>>#popup`, and `SpPopoverPresenter>>#popupPointingTo:`)
Also, it can be show at the top, left, bottom or right of the owner or rectangle to point.

The presenter placed on a popover needs to extend `SpPopoverContentPresenter` (or implement its API) to gain all properties.
"
Class {
	#name : #SpPopoverPresenter,
	#superclass : #SpAbstractWidgetPresenter,
	#instVars : [
		'#presenter => SpObservableSlot',
		'#position',
		'#relativeTo'
	],
	#category : #'Spec2-Core-Widgets'
}

{ #category : #specs }
SpPopoverPresenter class >> adapterName [

	^ #PopoverAdapter
]

{ #category : #documentation }
SpPopoverPresenter class >> documentFactoryMethodSelector [

	^ #newPopover
]

{ #category : #api }
SpPopoverPresenter >> bePositionBottom [
	"Set the popover to appear at the bottom of the owner (or the presenter set 
	 by `SpPopoverPresenter>>#relativeTo:`). 
	 See also `SpPopoverPresenter>>#position:`"

	self position: SpPopoverPosition bottom
]

{ #category : #api }
SpPopoverPresenter >> bePositionLeft [
	"Set the popover to appear at the left of the owner (or the presenter set 
	 by `SpPopoverPresenter>>#relativeTo:`)
	 See also `SpPopoverPresenter>>#position:`"

	self position: SpPopoverPosition left
]

{ #category : #api }
SpPopoverPresenter >> bePositionRight [
	"Set the popover to appear at the right of the owner (or the presenter set 
	 by `SpPopoverPresenter>>#relativeTo:`)
	 See also `SpPopoverPresenter>>#position:`"

	self position: SpPopoverPosition right
]

{ #category : #api }
SpPopoverPresenter >> bePositionTop [
	"Set the popover to appear at the top of the owner (or the presenter set 
	 by `SpPopoverPresenter>>#relativeTo:`)
	 See also `SpPopoverPresenter>>#position:`"

	self position: SpPopoverPosition top
]

{ #category : #'api-focus' }
SpPopoverPresenter >> defaultKeyboardFocus [

	^ self presenter
]

{ #category : #api }
SpPopoverPresenter >> dismiss [ 
	"Dismiss this popover"

	^ self withAdapterDo: [ :anAdapter | anAdapter dismiss ]
]

{ #category : #initialization }
SpPopoverPresenter >> initialize [

	super initialize.
	self bePositionBottom
]

{ #category : #api }
SpPopoverPresenter >> popup [
	"Shows the popover, relative to `SpPopoverPresenter>>#relativeTo`."

	self adapter ifNil: [ self buildWithSpec ].
	self withAdapterDo: [ :anAdapter |
		anAdapter popup ]
]

{ #category : #api }
SpPopoverPresenter >> popupPointingTo: aRectangle [
	"Shows the popover, relative to `SpPopoverPresenter>>#relativeTo`, and 
	 pointing to `aRectangle`"

	self adapter ifNil: [ self buildWithSpec ].
	self withAdapterDo: [ :anAdapter |
		anAdapter popupPointingTo: aRectangle ]
]

{ #category : #api }
SpPopoverPresenter >> position [
	"Answer the position (top, left, bottom, right) of the presenter."

	^ position
]

{ #category : #api }
SpPopoverPresenter >> position: aPosition [
	"Set the position (top, left, bottom, right) of the presenter."

	position := aPosition
]

{ #category : #api }
SpPopoverPresenter >> presenter [
	"Answer the presenter that will be shown by the popover."

	^ presenter
]

{ #category : #api }
SpPopoverPresenter >> presenter: aPresenter [
	"Set the presenter that will be shown by the popover."

	aPresenter owner: self.
	presenter := aPresenter
]

{ #category : #api }
SpPopoverPresenter >> relativeTo [
	"Answer the presenter that will show the popover (and the popover will be shown relative to it)."

	^ relativeTo ifNil: [ self owner ]
]

{ #category : #api }
SpPopoverPresenter >> relativeTo: aPresenter [
	"Set the presenter that will show the popover (and the popover will be shown relative to it)."

	relativeTo := aPresenter
]

{ #category : #'api-events' }
SpPopoverPresenter >> whenPresenterChangedDo: aBlock [
	"Inform when content presenter has changed. 
	 `aBlock` has three optional arguments: 
	 - new value
	 - old value
	 - the announcement triggering this action"

	self property: #presenter whenChangedDo: aBlock
]
