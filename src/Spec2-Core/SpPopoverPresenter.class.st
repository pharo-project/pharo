"
I'm a popover popover. 
I will be relative to my owner, but I can be shown pointing to a particular place (see popup, and popupPointingTo:)
Also, I can be show at the top, left, bottom or right of the owner or rectangle to point.

self example

The presenter placed on a popover needs to extent SpPopoverContentPresenter (or implement its api) to gain all properties.
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

{ #category : #api }
SpPopoverPresenter >> bePositionBottom [

	self position: SpPopoverPosition bottom
]

{ #category : #api }
SpPopoverPresenter >> bePositionLeft [

	self position: SpPopoverPosition left
]

{ #category : #api }
SpPopoverPresenter >> bePositionRight [

	self position: SpPopoverPosition right
]

{ #category : #api }
SpPopoverPresenter >> bePositionTop [

	self position: SpPopoverPosition top
]

{ #category : #'api-focus' }
SpPopoverPresenter >> defaultKeyboardFocus [

	^ self presenter
]

{ #category : #api }
SpPopoverPresenter >> dismiss [ 

	^ self withAdapterDo: [ :anAdapter | anAdapter dismiss ]
]

{ #category : #initialization }
SpPopoverPresenter >> initialize [

	super initialize.
	self bePositionBottom
]

{ #category : #api }
SpPopoverPresenter >> popup [
	"shows the popup, relative to #relativeTo"

	self adapter ifNil: [ self buildWithSpec ].
	self withAdapterDo: [ :anAdapter |
		anAdapter popup ]
]

{ #category : #api }
SpPopoverPresenter >> popupPointingTo: aRectangle [
	"shows the popup, relative to #relativeTo, and pointing to aRectangle"

	self adapter ifNil: [ self buildWithSpec ].
	self withAdapterDo: [ :anAdapter |
		anAdapter popupPointingTo: aRectangle ]
]

{ #category : #api }
SpPopoverPresenter >> position [

	^ position
]

{ #category : #api }
SpPopoverPresenter >> position: aPosition [

	position := aPosition
]

{ #category : #api }
SpPopoverPresenter >> presenter [
	^ presenter
]

{ #category : #api }
SpPopoverPresenter >> presenter: aPresenter [

	aPresenter owner: self.
	presenter := aPresenter
]

{ #category : #api }
SpPopoverPresenter >> relativeTo [

	^ relativeTo ifNil: [ self owner ]
]

{ #category : #api }
SpPopoverPresenter >> relativeTo: aPresenter [

	relativeTo := aPresenter
]

{ #category : #'api-events' }
SpPopoverPresenter >> whenPresenterChangedDo: aBlock [

	self property: #presenter whenChangedDo: aBlock
]
