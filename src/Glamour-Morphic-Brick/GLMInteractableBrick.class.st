"
A GLMInteractableBrick is a subclass of GLMBrick to allow user interactions such as: mouseLeave, mouseEnter, mouseDown, mouseUp and for each state can change background color or background image, that resizes accordinately to Brick's size. Supports doubleClick out of box, but it should be enabled sending enableDoubleClick message. When doubleClick is enabled announces #onDoubleClicked.

By default interactable brick is disabled and doesn't react to any user action. It should be first enabled sending enable message.

Instance Variables
	announcer:		<Announcer>
	checkedColor:		<Object>
	checkedImage:		<Object>
	isChecked:		<Boolean>
	isDoubleClickEnabled:		<Boolean>
	isEnabled:		<Boolean>
	isPressed:		<Boolean>
	isSelected:		<Boolean>
	normalColor:		<Object>
	normalImage:		<Object>
	pressedColor:		<Object>
	pressedImage:		<Object>
	selectedColor:		<Object>
	selectedImage:		<Object>

announcer
	- xxxxx

checkedColor
	- xxxxx

checkedImage
	- xxxxx

isChecked
	- xxxxx

isDoubleClickEnabled
	- xxxxx

isEnabled
	- xxxxx

isPressed
	- xxxxx

isSelected
	- xxxxx

normalColor
	- xxxxx

normalImage
	- xxxxx

pressedColor
	- xxxxx

pressedImage
	- xxxxx

selectedColor
	- xxxxx

selectedImage
	- xxxxx

"
Class {
	#name : #GLMInteractableBrick,
	#superclass : #GLMBrick,
	#instVars : [
		'normalImage',
		'pressedImage',
		'selectedImage',
		'checkedImage',
		'checkedColor',
		'normalColor',
		'pressedColor',
		'selectedColor',
		'isSelected',
		'isPressed',
		'isChecked',
		'isEnabled',
		'isDoubleClickEnabled'
	],
	#category : #'Glamour-Morphic-Brick-Widgets-Core'
}

{ #category : #states }
GLMInteractableBrick class >> normalColorOf: aTrick [
	^ self pressedColor: false selectedColor: false of: aTrick
]

{ #category : #states }
GLMInteractableBrick class >> normalOf: aTrick [
	^ self pressed: false selected: false of: aTrick
]

{ #category : #states }
GLMInteractableBrick class >> pressed: isPressed of: aTrick [
	^ self pressed: isPressed selected: false of: aTrick
]

{ #category : #states }
GLMInteractableBrick class >> pressed: isPressed selected: isSelected checked: isChecked of: aTrick [
	"answers sender with image, corresponding specified state"
	
	"pressed has the highest priority"
	(isPressed & aTrick pressedImage notNil)
		ifTrue: [ ^ aTrick pressedImage ].
	
	(isChecked & aTrick checkedImage notNil)
		ifTrue: [ ^ aTrick checkedImage ].
		
	(((isPressed & aTrick pressedImage isNil) | isSelected) & aTrick selectedImage notNil)
		ifTrue: [ ^ aTrick selectedImage ].
	
	^ aTrick normalImage.
]

{ #category : #states }
GLMInteractableBrick class >> pressed: isPressed selected: isSelected of: aTrick [
	^ self pressed: isPressed selected: isSelected checked: false of: aTrick
]

{ #category : #states }
GLMInteractableBrick class >> pressedColor: isPressed of: aTrick [
	^ self pressedColor: isPressed selectedColor: false of: aTrick
]

{ #category : #states }
GLMInteractableBrick class >> pressedColor: isPressed selectedColor: isSelected checkedColor: isChecked of:aTrick [
	"answers sender with image, corresponding specified state"
	
	"pressed has the highest priority"
	(isPressed & aTrick pressedColor notNil)
		ifTrue: [ ^ aTrick pressedColor ].
		
	(isChecked & aTrick checkedColor notNil)
		ifTrue: [ ^ aTrick checkedColor ].
		
	(((isPressed & aTrick pressedColor isNil) | isSelected) & aTrick selectedColor notNil)
		ifTrue: [ ^ aTrick selectedColor ].

	
	^ aTrick normalColor.
]

{ #category : #states }
GLMInteractableBrick class >> pressedColor: isPressed selectedColor: isSelected of: aTrick [
	^ self pressedColor: isPressed selectedColor: isSelected checkedColor: false of: aTrick
]

{ #category : #states }
GLMInteractableBrick class >> selected: isSelected of: aTrick [
	^ self pressed: false selected: isSelected of: aTrick
]

{ #category : #states }
GLMInteractableBrick class >> selectedColor: isSelected of: aTrick [
	^ self pressedColor: false selectedColor: isSelected of: aTrick
]

{ #category : #'brick-interactable-actions' }
GLMInteractableBrick >> check [
	"notifies #onChecked"
	self isChecked ifFalse: [
		isChecked := true.
		self announce: #onChecked.
		self changed ]
]

{ #category : #'brick-interactable-actions' }
GLMInteractableBrick >> check: aBoolean [
	aBoolean
		ifTrue: [ self check ]
		ifFalse: [ self uncheck ]
]

{ #category : #'brick-interactable-accessing' }
GLMInteractableBrick >> checkedColor [
	^ checkedColor
]

{ #category : #'brick-interactable-accessing' }
GLMInteractableBrick >> checkedColor: anObject [
	checkedColor := anObject
]

{ #category : #'brick-interactable-accessing' }
GLMInteractableBrick >> checkedImage [
	^ checkedImage
]

{ #category : #'brick-interactable-accessing' }
GLMInteractableBrick >> checkedImage: anObject [
	checkedImage := anObject
]

{ #category : #'brick-interactable-actions' }
GLMInteractableBrick >> click: anEvent [
	self announce: #onClicked event: anEvent.
	^ false
]

{ #category : #'brick-interactable-accessing' }
GLMInteractableBrick >> currentColor [
	^ self state pressedColor: (self isPressed) selectedColor: (self isSelected) checkedColor: (self isChecked) of: self
]

{ #category : #'brick-interactable-accessing' }
GLMInteractableBrick >> currentImage [
	^ self state pressed: (self isPressed) selected: (self isSelected) checked: (self isChecked) of: self
]

{ #category : #initialization }
GLMInteractableBrick >> defaultRenderer [
	^ GLMInteractableBrickRenderer new
]

{ #category : #'brick-interactable-actions' }
GLMInteractableBrick >> depress [
	self isPressed ifTrue: [
		isPressed := false.
		self changed.
		^ true ].
	^ false
]

{ #category : #'brick-interactable-actions' }
GLMInteractableBrick >> deselect [
	"notifies #onMouseUnhover"
	self isSelected ifTrue: [
		isSelected := false.
		self announce: #onMouseUnhover.
		self changed ]
]

{ #category : #'brick-interactable-actions' }
GLMInteractableBrick >> deselect: anEvent [

	self deselect
]

{ #category : #'brick-interactable-actions' }
GLMInteractableBrick >> disable [
	isEnabled := false.
]

{ #category : #'brick-interactable-actions' }
GLMInteractableBrick >> doubleClick: anEvent [

	self announce: #onDoubleClicked event: anEvent.
	^ false
]

{ #category : #'brick-interactable-actions' }
GLMInteractableBrick >> enable [
	
	isEnabled := true.
]

{ #category : #'brick-interactable-actions' }
GLMInteractableBrick >> enable: aBoolean [
	
	isEnabled := aBoolean.
]

{ #category : #'brick-interactable-actions' }
GLMInteractableBrick >> enableDoubleClick [

	isDoubleClickEnabled := true
]

{ #category : #'brick-morph-mouse-events' }
GLMInteractableBrick >> handlesMouseDown: evt [
	
	^ self isEnabled
		ifTrue: [ true ]
		ifFalse: [ super handlesMouseDown: evt ]
	
]

{ #category : #'brick-morph-mouse-events' }
GLMInteractableBrick >> handlesMouseOver: evt [
	^ self isEnabled
		ifTrue: [ true ]
		ifFalse: [ super handlesMouseDown: evt ]
]

{ #category : #initialization }
GLMInteractableBrick >> initialize [
	super initialize.
	
	isChecked := false.
	isPressed := false.
	isSelected := false.
	
	self disable.
]

{ #category : #'brick-interactable-testing' }
GLMInteractableBrick >> isChecked [
	^ isChecked.
]

{ #category : #'brick-interactable-testing' }
GLMInteractableBrick >> isDoubleClickEnabled [

	^ isDoubleClickEnabled ifNil: [ isDoubleClickEnabled := false ]
]

{ #category : #'brick-interactable-testing' }
GLMInteractableBrick >> isEnabled [
	^ isEnabled
]

{ #category : #'brick-interactable-testing' }
GLMInteractableBrick >> isFocused [

	^ false
]

{ #category : #'brick-interactable-testing' }
GLMInteractableBrick >> isHovered [
	^ self isSelected
]

{ #category : #'brick-interactable-testing' }
GLMInteractableBrick >> isPressed [
	^ isPressed.
]

{ #category : #'brick-interactable-testing' }
GLMInteractableBrick >> isSelected [
	^ isSelected.
]

{ #category : #'brick-morph-mouse-events' }
GLMInteractableBrick >> mouseDown: evt [
	|selectors|
	
	self isDoubleClickEnabled ifFalse: [
		super mouseDown: evt.
		^ self press ].
	
	selectors := Array
				with: #click:
				with: #doubleClick:
				with: #singleClick:
				with: nil.
				
	evt hand
		waitForClicksOrDrag: self
		event: evt
		selectors: selectors
		threshold: 10.

]

{ #category : #'brick-morph-mouse-events' }
GLMInteractableBrick >> mouseEnter: evt [
	super mouseEnter: evt.
	^ self select: evt
]

{ #category : #'brick-morph-mouse-events' }
GLMInteractableBrick >> mouseLeave: evt [
	super mouseLeave: evt.
	^ self deselect: evt
]

{ #category : #'brick-morph-mouse-events' }
GLMInteractableBrick >> mouseUp: evt [
	super mouseUp: evt.
	(self bounds containsPoint: evt position) & (self isPressed) ifTrue: [
		self depress.
		^ self click: evt
	] ifFalse: [
		self deselect: evt.
		^ self depress ]
]

{ #category : #'brick-interactable-accessing' }
GLMInteractableBrick >> normalColor [
	^ normalColor
]

{ #category : #'brick-interactable-accessing' }
GLMInteractableBrick >> normalColor: anObject [
	normalColor := anObject
]

{ #category : #'brick-interactable-accessing' }
GLMInteractableBrick >> normalImage [
	^ normalImage
]

{ #category : #'brick-interactable-accessing' }
GLMInteractableBrick >> normalImage: anObject [
	normalImage := anObject
]

{ #category : #'brick-interactable-actions' }
GLMInteractableBrick >> press [
	self isPressed ifFalse: [
		isPressed := true.
		self changed.
	].

	^ false
]

{ #category : #'brick-interactable-accessing' }
GLMInteractableBrick >> pressedColor [
	^ pressedColor
]

{ #category : #'brick-interactable-accessing' }
GLMInteractableBrick >> pressedColor: anObject [
	pressedColor := anObject
]

{ #category : #'brick-interactable-accessing' }
GLMInteractableBrick >> pressedImage [
	^ pressedImage
]

{ #category : #'brick-interactable-accessing' }
GLMInteractableBrick >> pressedImage: anObject [
	pressedImage := anObject
]

{ #category : #'brick-interactable-actions' }
GLMInteractableBrick >> select [
	"notifies #onMouseHover"
	self isSelected ifFalse: [ 
		isSelected := true.
		self announce: #onMouseHover.
		self changed ]
]

{ #category : #'brick-interactable-actions' }
GLMInteractableBrick >> select: anEvent [

	self select
]

{ #category : #'brick-interactable-accessing' }
GLMInteractableBrick >> selectedColor [
	^ selectedColor
]

{ #category : #'brick-interactable-accessing' }
GLMInteractableBrick >> selectedColor: anObject [
	selectedColor := anObject
]

{ #category : #'brick-interactable-accessing' }
GLMInteractableBrick >> selectedImage [
	^ selectedImage
]

{ #category : #'brick-interactable-accessing' }
GLMInteractableBrick >> selectedImage: anObject [
	selectedImage := anObject
]

{ #category : #'brick-interactable-actions' }
GLMInteractableBrick >> singleClick: anEvent [

	self announce: #onSingleClicked event: anEvent
]

{ #category : #'brick-interactable-accessing' }
GLMInteractableBrick >> state [

	^ self class
]

{ #category : #'brick-interactable-actions' }
GLMInteractableBrick >> uncheck [
	"notifies #onUnchecked"
	self isChecked ifTrue: [
		isChecked := false.
		self announce: #onUnchecked.
		self changed ]
]
