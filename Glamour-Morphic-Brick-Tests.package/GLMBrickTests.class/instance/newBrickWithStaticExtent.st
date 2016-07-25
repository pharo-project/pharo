instance-creation
newBrickWithStaticExtent

	^ GLMBrick new 
		height: self staticHeight;
		width: self staticWidth;
		color: Color random;
		yourself