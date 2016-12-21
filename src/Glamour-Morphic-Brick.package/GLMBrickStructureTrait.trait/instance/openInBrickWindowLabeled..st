brick-structure-opening
openInBrickWindowLabeled: aLabel
	
	^ self class environment at: #GLMSystemWindowBrick 
		ifPresent: [ :glmSystemWindowBrick |
			glmSystemWindowBrick new
				label: aLabel;
				color: Color transparent;
				addBrickBack: (
					GLMBrick new
						vSpaceFill
						hSpaceFill;
						addBrickBack: self;
						yourself);
				extent: 400@400;
				openCenteredInWorld ]
		ifAbsent: [ self asMorph openInWindow ] 
	