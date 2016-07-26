label-styles
createMenuBoxFor: aSystemWindow
	"Answer a button for the window menu."
	
	|form msb|
	form := self windowMenuForm.
	msb := MultistateButtonMorph new extent: form extent.
	msb activeEnabledNotOverUpFillStyle: (ImageFillStyle form: form).
	form := self windowMenuPassiveForm.
	msb extent: form extent.
	msb passiveEnabledNotOverUpFillStyle: (ImageFillStyle form: form).
	form := self windowMenuOverForm.
	msb extent: form extent.
	msb
		activeEnabledOverUpFillStyle: (ImageFillStyle form: form);
		passiveEnabledOverUpFillStyle: (ImageFillStyle form: form).
	form := self windowMenuPassiveForm.
	msb
		extent: form extent;
		activeEnabledOverDownFillStyle: (ImageFillStyle form: form);
		passiveEnabledOverDownFillStyle: (ImageFillStyle form: form);
		addUpAction: [aSystemWindow offerWindowMenu];
		setBalloonText: 'Window menu' translated;
		extent: form extent.
	^msb