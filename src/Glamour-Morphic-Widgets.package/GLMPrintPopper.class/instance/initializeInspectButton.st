initialization
initializeInspectButton

	inspectButton := MultistateButtonMorph new
		extent: GLMUIThemeExtraIcons glamorousInspect extent;
		activeEnabledOverUpFillStyle: (ImageFillStyle form: GLMUIThemeExtraIcons glamorousInspect);
		setBalloonText: 'Inspect' translated.
	self
		addMorph: inspectButton
		fullFrame: (LayoutFrame identity 
			leftFraction: 1;
			bottomFraction: 0;
			leftOffset: inspectButton width;
			bottomOffset: (2*inspectButton width))