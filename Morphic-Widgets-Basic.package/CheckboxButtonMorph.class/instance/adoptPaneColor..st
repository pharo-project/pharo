accessing
adoptPaneColor: paneColor
	"Pass on to the border too."
	
	super adoptPaneColor: paneColor.
	paneColor ifNil: [^self].
	self fillStyle: self fillStyleToUse.
	self borderStyle: self borderStyleToUse.
	self cornerStyle: (self isRadioButton
		ifTrue: [self theme radioButtonCornerStyleFor: self]
		ifFalse: [self theme checkboxCornerStyleFor: self])