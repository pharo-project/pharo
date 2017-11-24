"
Specially themed ""control"" button. Used for drop-lists, expanders etc.
"
Class {
	#name : #ControlButtonMorph,
	#superclass : #PluggableButtonMorph,
	#category : #'Morphic-Widgets-Basic-Buttons'
}

{ #category : #accessing }
ControlButtonMorph >> disabledBorderStyle [
	"Return the disabled borderStyle of the receiver."
	
	^self theme controlButtonDisabledBorderStyleFor: self
]

{ #category : #accessing }
ControlButtonMorph >> disabledFillStyle [
	"Return the disabled fillStyle of the receiver."
	
	^self theme controlButtonDisabledFillStyleFor: self
]

{ #category : #initialization }
ControlButtonMorph >> initialize [
	"Initialize the receiver."

	super initialize.
	self layoutInset: (self theme controlButtonLabelInsetFor: self)
]

{ #category : #accessing }
ControlButtonMorph >> minWidth [
	"Consult the theme also."

	^self perform: #minWidth withArguments: #() inSuperclass: Morph
]

{ #category : #style }
ControlButtonMorph >> mouseOverBorderStyle [
	"Return the mouse over borderStyle of the receiver."
	
	^self theme controlButtonMouseOverBorderStyleFor: self
]

{ #category : #style }
ControlButtonMorph >> mouseOverFillStyle [
	"Return the mouse over fillStyle of the receiver."
	
	^self theme controlButtonMouseOverFillStyleFor: self
]

{ #category : #style }
ControlButtonMorph >> normalBorderStyle [
	"Return the normal borderStyle of the receiver."
	
	^self theme controlButtonNormalBorderStyleFor: self
]

{ #category : #style }
ControlButtonMorph >> normalFillStyle [
	"Return the normal fillStyle of the receiver."
	
	^self theme controlButtonNormalFillStyleFor: self
]

{ #category : #style }
ControlButtonMorph >> pressedBorderStyle [
	"Return the pressed borderStyle of the receiver."
	
	^self theme controlButtonPressedBorderStyleFor: self
]

{ #category : #style }
ControlButtonMorph >> pressedFillStyle [
	"Return the pressed fillStyle of the receiver."
	
	^self theme controlButtonPressedFillStyleFor: self
]

{ #category : #accessing }
ControlButtonMorph >> selectedDisabledBorderStyle [
	"Return the selected disabled borderStyle of the receiver."
	
	^self theme controlButtonSelectedDisabledBorderStyleFor: self
]

{ #category : #style }
ControlButtonMorph >> selectedDisabledFillStyle [
	"Return the selected disabled fillStyle of the receiver."
	
	^self theme controlButtonSelectedDisabledFillStyleFor: self
]

{ #category : #style }
ControlButtonMorph >> selectedFillStyle [
	"Return the selected fillStyle of the receiver."
	
	^self theme controlButtonSelectedFillStyleFor: self
]

{ #category : #style }
ControlButtonMorph >> selectedMouseOverBorderStyle [
	"Return the selected mouse over borderStyle of the receiver."
	
	^self theme controlButtonSelectedMouseOverBorderStyleFor: self
]

{ #category : #style }
ControlButtonMorph >> selectedMouseOverFillStyle [
	"Return the selected mouse over fillStyle of the receiver."
	
	^self theme controlButtonSelectedMouseOverFillStyleFor: self
]

{ #category : #style }
ControlButtonMorph >> selectedPressedBorderStyle [
	"Return the selected pressed borderStyle of the receiver."
	
	^self theme controlButtonSelectedPressedBorderStyleFor: self
]

{ #category : #style }
ControlButtonMorph >> selectedPressedFillStyle [
	"Return the selected pressed fillStyle of the receiver."
	
	^self theme controlButtonSelectedPressedFillStyleFor: self
]
