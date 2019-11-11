Class {
	#name : #GLMPagerScrollButtonBrick,
	#superclass : #GLMInteractableBrick,
	#instVars : [
		'checkedIcon',
		'checkedFocusedIcon',
		'focused',
		'index'
	],
	#category : #'Glamour-Morphic-Pager-Brick'
}

{ #category : #accessing }
GLMPagerScrollButtonBrick >> checkedFocusedIcon [
	^ checkedFocusedIcon
]

{ #category : #accessing }
GLMPagerScrollButtonBrick >> checkedFocusedIcon: aForm [
	checkedFocusedIcon := aForm.
]

{ #category : #accessing }
GLMPagerScrollButtonBrick >> checkedIcon [
	^ checkedIcon
]

{ #category : #accessing }
GLMPagerScrollButtonBrick >> checkedIcon: aForm [
	checkedIcon := aForm.
]

{ #category : #'brick-interactable-actions' }
GLMPagerScrollButtonBrick >> click: anEvent [
	super click: anEvent.
	
	self announcer announce: (GLMPagerButtonClicked new buttonModel: self)
	
]

{ #category : #'brick-interactable-actions' }
GLMPagerScrollButtonBrick >> deselect [
	super deselect.
	
	self announcer announce: (GLMPagerButtonDeselected new buttonModel: self)
	
]

{ #category : #accessing }
GLMPagerScrollButtonBrick >> index [
	^ index
]

{ #category : #accessing }
GLMPagerScrollButtonBrick >> index: anObject [
	index := anObject
]

{ #category : #initialization }
GLMPagerScrollButtonBrick >> initialize [
	super initialize.

	focused := false.
]

{ #category : #'brick-interactable-testing' }
GLMPagerScrollButtonBrick >> isFocused [
	^ focused
]

{ #category : #'brick-interactable-actions' }
GLMPagerScrollButtonBrick >> select [
	super select.
	
	self announcer announce: (GLMPagerButtonSelected new buttonModel: self)
	
]

{ #category : #initialization }
GLMPagerScrollButtonBrick >> setFocused [

	focused ifFalse: [
		self checkedImage: checkedFocusedIcon.
		self check.
		focused := true ]
]

{ #category : #initialization }
GLMPagerScrollButtonBrick >> setNotFocused [

	focused ifTrue: [
		self checkedImage: checkedIcon.
		self uncheck.
		focused := false ]
]
