label-styles
createMenuBoxFor: aSystemWindow
	"Answer a button for the window menu."
	
"	^aSystemWindow createBox
		labelGraphic: (self windowMenuIconFor: aSystemWindow);
		extent: aSystemWindow boxExtent;
		actWhen: #buttonDown;
		actionSelector: #offerWindowMenu;
		setBalloonText: 'window menu' translated"
	
	|form msb|
	form := self windowMenuForm.
	msb := MultistateButtonMorph new extent: form extent.
	msb activeEnabledNotOverUpFillStyle: (ImageFillStyle form: form).
	form := self windowMenuPassiveForm.
	msb extent: form extent.
	msb activeDisabledNotOverUpFillStyle: (ImageFillStyle form: form).
	msb passiveEnabledNotOverUpFillStyle: (ImageFillStyle form: form).
	msb passiveDisabledNotOverUpFillStyle: (ImageFillStyle form: form).
	form := self windowMenuForm.
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
		setBalloonText: 'window menu' translated;
		extent: aSystemWindow boxExtent.
	^msb