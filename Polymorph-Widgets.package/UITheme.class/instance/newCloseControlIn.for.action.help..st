morph creation
newCloseControlIn: aThemedMorph for: aModel action: aValuable help: helpText
	"Answer a button for closing things."
	
	|form msb|
	form := self windowCloseForm.
	msb := MultistateButtonMorph new extent: form extent.
	msb
		activeEnabledNotOverUpFillStyle: (ImageFillStyle form: form);
		addUpAction: aValuable;
		setBalloonText: helpText.
	^msb