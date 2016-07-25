examples
gtExampleZindex
	<gtExample>
	<label: 'Example of z-index support in Brick'>
	
	^ GLMBrick new
		hSpaceFill;
		vSpaceFill;
		color: Color blue;
		addBrickBack: (
			GLMBrick new
				color: Color green;
				brickBounds: (100@100 extent: 100@100);
				zIndex: 3);
		addBrickBack: (
			GLMBrick new
				color: Color yellow;
				brickBounds: (50@50 extent: 200@100);
				zIndex: 2);
		addBrickBack: (
			GLMBrick new
				color: Color red;
				brickBounds: (0@0 extent: 300@200);
				zIndex: 1);
		yourself