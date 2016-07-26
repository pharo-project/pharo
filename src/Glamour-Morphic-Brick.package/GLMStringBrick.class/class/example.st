as yet unclassified
example

	| brick brick1 brick2 brick3|
	brick := GLMBrick new
		color: Color blue;
		width: 400;
		height: 300.
		
	brick1 := self new
		color: Color red;
		hSpaceFill: 75;
		vSpaceFill: 25;
		textPosition: #center;
		text: 'Brick 1'.
	
	brick2 := self new
		color: Color green;
		hSpaceFill: 25;
		vSpaceFill: 75;
		textPosition: #center;
		text: 'Brick 2'.
	
	brick3 := self new
		color: Color yellow;
		width: 50;
		height: 50;
		textPosition: #center;
		text: 'Brick 3'.
	
	brick
		addBrickBack: brick1;
		addBrickBack: brick2;
		addBrickBack: brick3.
		
	brick asBrickedMorph
		color: Color white;
		openInWindow