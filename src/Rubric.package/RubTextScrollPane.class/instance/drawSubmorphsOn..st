drawing
drawSubmorphsOn: aCanvas 
	"Draw the focus here since we are using inset bounds
	for the focus rectangle."

	super drawSubmorphsOn: aCanvas.
	self textArea readOnly ifTrue: [ ^self ].
	self drawFocusBorder ifFalse: [ ^ self ].
	(self hasKeyboardFocus or: [ self textArea hasFindReplaceFocus ]) 
		ifTrue: [self drawKeyboardFocusOn: aCanvas].
