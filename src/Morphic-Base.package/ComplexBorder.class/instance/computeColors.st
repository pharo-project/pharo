private
computeColors
	width = 0 ifTrue:[^colors := #()].
	style == #complexFramed ifTrue:[^self computeFramedColors].
	style == #complexAltFramed ifTrue:[^self computeAltFramedColors].
	style == #complexRaised ifTrue:[^self computeRaisedColors].
	style == #complexAltRaised ifTrue:[^self computeAltRaisedColors].
	style == #complexInset ifTrue:[^self computeInsetColors].
	style == #complexAltInset ifTrue:[^self computeAltInsetColors].
	self error:'Unknown border style: ', style printString.