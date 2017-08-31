instance creation
forAction: anAction
	| baseColor |
	
	baseColor := Smalltalk ui theme baseColor.
	self extent: anAction icon extent.
	
	self activeEnabledNotOverUpFillStyle: baseColor.
	
	self activeDisabledNotOverUpFillStyle: (self theme buttonDisabledFillStyleFor: self).
	self passiveEnabledNotOverUpFillStyle: baseColor.
	self passiveDisabledNotOverUpFillStyle: baseColor.
	
	self
		activeEnabledOverUpFillStyle: (self theme buttonMouseOverFillStyleFor: self);
		passiveEnabledOverUpFillStyle: (self theme buttonMouseOverFillStyleFor: self).
	
	self
		activeEnabledOverDownFillStyle: (self theme buttonPressedFillStyleFor: self);
		passiveEnabledOverDownFillStyle: (self theme buttonPressedFillStyleFor: self);
		addUpAction: [ anAction action value ];
		setBalloonText: anAction label;
		extent: 16@16.
		
	icon := anAction icon.
		
	^ self