Class {
	#name : #GLMPagerScrollButtonBrick,
	#superclass : #GLMInteractableBrick,
	#instVars : [
		'checkedIcon',
		'checkedFocusedIcon',
		'focused',
		'index'
	],
	#category : 'Glamour-Morphic-Pager-Brick'
}

{ #category : #'as yet unclassified' }
GLMPagerScrollButtonBrick >> checkedFocusedIcon [
	^ checkedFocusedIcon
]

{ #category : #'as yet unclassified' }
GLMPagerScrollButtonBrick >> checkedFocusedIcon: aForm [
	checkedFocusedIcon := aForm.
]

{ #category : #'as yet unclassified' }
GLMPagerScrollButtonBrick >> checkedIcon [
	^ checkedIcon
]

{ #category : #'as yet unclassified' }
GLMPagerScrollButtonBrick >> checkedIcon: aForm [
	checkedIcon := aForm.
]

{ #category : #'as yet unclassified' }
GLMPagerScrollButtonBrick >> click: anEvent [
	super click: anEvent.
	
	self announcer announce: (GLMPagerButtonClicked new buttonModel: self)
	
]

{ #category : #'as yet unclassified' }
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

{ #category : #'as yet unclassified' }
GLMPagerScrollButtonBrick >> initialize [
	super initialize.

	focused := false.
]

{ #category : #'as yet unclassified' }
GLMPagerScrollButtonBrick >> isFocused [
	^ focused
]

{ #category : #'as yet unclassified' }
GLMPagerScrollButtonBrick >> select [
	super select.
	
	self announcer announce: (GLMPagerButtonSelected new buttonModel: self)
	
]

{ #category : #'as yet unclassified' }
GLMPagerScrollButtonBrick >> setFocused [

	focused ifFalse: [
		self checkedImage: checkedFocusedIcon.
		self check.
		focused := true ]
]

{ #category : #'as yet unclassified' }
GLMPagerScrollButtonBrick >> setNotFocused [

	focused ifTrue: [
		self checkedImage: checkedIcon.
		self uncheck.
		focused := false ]
]
