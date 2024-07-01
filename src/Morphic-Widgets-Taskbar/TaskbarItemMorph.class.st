"
Description
--------------------

A TaskbarItemMorph is a Morph representing a unique item of the taskbar. 

It can be customized via the current theme for its border and fill style.

Examples
--------------------

	(TaskbarItemMorph on: aWindow getState: #isActive action: #taskbarButtonClicked)
		initializeFor: aTaskbar;
		yourself
"
Class {
	#name : 'TaskbarItemMorph',
	#superclass : 'PluggableButtonMorph',
	#category : 'Morphic-Widgets-Taskbar',
	#package : 'Morphic-Widgets-Taskbar'
}

{ #category : 'customization' }
TaskbarItemMorph >> addEmphaseTo: lab [
	"1 -> bold"

	self model isActive ifFalse: [ ^ self ].

	lab emphasis: 1
]

{ #category : 'style - border' }
TaskbarItemMorph >> disabledBorderStyle [
	"Return the disabled borderStyle of the receiver."

	^ self theme taskbarItemDisabledBorderStyleFor: self
]

{ #category : 'style - filling' }
TaskbarItemMorph >> disabledFillStyle [
	"Return the disabled fillStyle of the receiver."

	^ self theme taskbarItemDisabledFillStyleFor: self
]

{ #category : 'initialization' }
TaskbarItemMorph >> initialize [
	super initialize.
	self borderStyle: (self theme taskbarItemNormalBorderStyleFor: self)
]

{ #category : 'initialization' }
TaskbarItemMorph >> initializeAnnouncements [
	self
		on: #mouseEnter send: #taskbarButtonEntered:event:in: to: self model withValue: self;
		on: #mouseLeave send: #taskbarButtonLeft:event:in: to: self model withValue: self
]

{ #category : 'initialization' }
TaskbarItemMorph >> initializeFor: aTaskbar [
	| lm lab labSize |

	labSize := self labelSizeIn: aTaskbar.
	lab := self labelOfSize: labSize.
	self addEmphaseTo: lab.
	lm := self theme
		newRowIn: aTaskbar
		for:
			{ (self model taskbarIcon ifNil: [ ^ nil ]) asMorph.
			lab}.

	lm cellInset: 2.
	self
		label: lm font: self theme buttonFont;
		extent: self minExtent;
		hResizing: #rigid;
		vResizing: #rigid;
		useSquareCorners;
		getMenuSelector: #taskbarButtonMenu:.
	self initializeAnnouncements.
	lab
		color:
			(self model isCollapsed
				ifTrue: [ self theme taskbarItemLabelColorForCollapsed: self ]
				ifFalse: [ self theme taskbarItemLabelColorForExpanded: self ])
]

{ #category : 'accessing' }
TaskbarItemMorph >> labelOfSize: labSize [
	^ self theme	buttonLabelForText: (self model taskbarLabel truncateWithElipsisTo: labSize)
]

{ #category : 'accessing' }
TaskbarItemMorph >> labelSizeIn: aTaskbar [
	^ (150 // (aTaskbar tasks size + 1) max: 10) min: 30
]

{ #category : 'style - border' }
TaskbarItemMorph >> mouseOverBorderStyle [
	"Return the mouse over borderStyle of the receiver."

	^ self theme taskbarItemMouseOverBorderStyleFor: self
]

{ #category : 'style - filling' }
TaskbarItemMorph >> mouseOverFillStyle [
	"Return the mouse over fillStyle of the receiver."

	^ self theme taskbarItemMouseOverFillStyleFor: self
]

{ #category : 'style - border' }
TaskbarItemMorph >> normalBorderStyle [
	"Return the normal borderStyle of the receiver."

	^ self theme taskbarItemNormalBorderStyleFor: self
]

{ #category : 'style - filling' }
TaskbarItemMorph >> normalFillStyle [
	"Return the normal fillStyle of the receiver."

	^ self theme taskbarItemNormalFillStyleFor: self
]

{ #category : 'style - border' }
TaskbarItemMorph >> pressedBorderStyle [
	"Return the pressed borderStyle of the receiver."

	^ self theme taskbarItemPressedBorderStyleFor: self
]

{ #category : 'style - filling' }
TaskbarItemMorph >> pressedFillStyle [
	"Return the pressed fillStyle of the receiver."

	^ self theme taskbarItemPressedFillStyleFor: self
]

{ #category : 'style - border' }
TaskbarItemMorph >> selectedBorderStyle [
	"Return the selected borderStyle of the receiver."

	^ self theme taskbarItemSelectedBorderStyleFor: self
]

{ #category : 'style - border' }
TaskbarItemMorph >> selectedDisabledBorderStyle [
	"Return the selected disabled borderStyle of the receiver."

	^ self theme taskbarItemSelectedDisabledBorderStyleFor: self
]

{ #category : 'style - filling' }
TaskbarItemMorph >> selectedDisabledFillStyle [
	"Return the selected disabled fillStyle of the receiver."

	^ self theme taskbarItemSelectedDisabledFillStyleFor: self
]

{ #category : 'style - filling' }
TaskbarItemMorph >> selectedFillStyle [
	"Return the selected fillStyle of the receiver."

	^ self theme taskbarItemSelectedFillStyleFor: self
]

{ #category : 'style - border' }
TaskbarItemMorph >> selectedMouseOverBorderStyle [
	"Return the selected mouse over borderStyle of the receiver."

	^ self theme taskbarItemSelectedMouseOverBorderStyleFor: self
]

{ #category : 'style - filling' }
TaskbarItemMorph >> selectedMouseOverFillStyle [
	"Return the selected mouse over fillStyle of the receiver."

	^ self theme taskbarItemSelectedMouseOverFillStyleFor: self
]

{ #category : 'style - border' }
TaskbarItemMorph >> selectedPressedBorderStyle [
	"Return the selected pressed borderStyle of the receiver."

	^ self theme taskbarItemSelectedPressedBorderStyleFor: self
]

{ #category : 'style - filling' }
TaskbarItemMorph >> selectedPressedFillStyle [
	"Return the selected pressed fillStyle of the receiver."

	^ self theme taskbarItemSelectedPressedFillStyleFor: self
]

{ #category : 'menu' }
TaskbarItemMorph >> wantsYellowButtonMenu [
	^ true
]
