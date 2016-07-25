accessing
borderStyleToUse
	"Return the borderStyle to use for the receiver."
	
	self gradientLook ifFalse:[^self borderStyle].
	^self perform: (self availableBorderStyles at: (
			self interactionStates indexOf: self interactionState))
