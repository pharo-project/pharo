brick-structure-opening
openInBrickWindowLabeled: aLabel
	
	#GLMSystemWindowBrick asClassIfAbsent: [ 
		^ self asMorph openInWindow ].
	
	^ #GLMSystemWindowBrick asClass new
		label: aLabel;
		color: Color transparent;
		addBrickBack: (
			GLMBrick new
				vSpaceFill
				hSpaceFill;
				addBrickBack: self;
				yourself);
		extent: 400@400;
		openCenteredInWorld