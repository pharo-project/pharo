private
createMenuButton
	"Answer a button for the window menu."
	| form msb |
	
	form := Smalltalk ui theme windowMenuForm.
	msb := MultistateButtonMorph new extent: form extent.
	msb activeEnabledNotOverUpFillStyle: (ImageFillStyle form: form).
	
	form := Smalltalk ui theme windowMenuPassiveForm.
	msb extent: form extent.
	msb activeDisabledNotOverUpFillStyle: (ImageFillStyle form: form).
	msb passiveEnabledNotOverUpFillStyle: (ImageFillStyle form: form).
	msb passiveDisabledNotOverUpFillStyle: (ImageFillStyle form: form).
	
	form := Smalltalk ui theme windowMenuForm.
	msb extent: form extent.
	msb
		activeEnabledOverUpFillStyle: (ImageFillStyle form: form);
		passiveEnabledOverUpFillStyle: (ImageFillStyle form: form).
	
	form := Smalltalk ui theme windowMenuPassiveForm.
	msb
		extent: form extent;
		activeEnabledOverDownFillStyle: (ImageFillStyle form: form);
		passiveEnabledOverDownFillStyle: (ImageFillStyle form: form);
		addUpAction: [ self popUpMenu ];
		setBalloonText: 'tab menu' translated;
		extent: 16@16.
		
	^ msb