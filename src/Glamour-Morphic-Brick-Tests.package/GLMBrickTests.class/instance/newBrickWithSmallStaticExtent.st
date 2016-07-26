instance-creation
newBrickWithSmallStaticExtent

	^ GLMBrick new 
		height: self staticSmallHeight;
		width: self staticSmallWidth;
		color: Color random;
		yourself