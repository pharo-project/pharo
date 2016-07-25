private
dropDownMenuFrom: aBlock withIcon: anIcon
	| activeForm passiveForm menuButton | 
	
	activeForm := anIcon.
	passiveForm := anIcon.
	menuButton := MultistateButtonMorph new extent: activeForm extent.
	^ menuButton
		extent: activeForm extent;
				activeEnabledOverUpFillStyle: (ImageFillStyle form: activeForm);
				passiveEnabledOverUpFillStyle: (ImageFillStyle form: activeForm);
				activeEnabledOverDownFillStyle: (ImageFillStyle form: passiveForm);
		passiveEnabledOverDownFillStyle: (ImageFillStyle form: passiveForm);
		addUpAction: aBlock;
		setBalloonText: 'Menu' translated;
		yourself

