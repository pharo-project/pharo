initialization
initializeInspectButton

	inspectButton := MultistateButtonMorph new
		extent: GLMUIThemeExtraIcons glamorousInspect extent;
		activeEnabledOverUpFillStyle: (ImageFillStyle form: GLMUIThemeExtraIcons glamorousInspect);
		setBalloonText: 'Inspect' translated.
	self
		addMorph: inspectButton
		fullFrame: (LayoutFrame fractions: (1 @ 0 corner: 1 @ 0) offsets: (inspectButton width  @ 0 corner: 0 @ (2*inspectButton width) ))