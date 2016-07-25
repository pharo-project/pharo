styling
style: aText
	self terminateBackgroundStylingProcess.
	stylingEnabled
		ifFalse: [ ^ self ].
	text := aText copy.
	self privateStyle: text.
	view ifNotNil: [ :view2 | view2 stylerStyled: text ]