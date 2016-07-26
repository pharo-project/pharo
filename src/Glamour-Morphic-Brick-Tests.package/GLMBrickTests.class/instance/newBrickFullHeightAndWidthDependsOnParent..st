instance-creation
newBrickFullHeightAndWidthDependsOnParent: anInteger

	^ GLMBrick new
		vSpaceFill;
		hSpaceFill: anInteger;
		color: Color random;
		yourself