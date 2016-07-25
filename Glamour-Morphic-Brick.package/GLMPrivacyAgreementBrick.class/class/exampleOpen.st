examples
exampleOpen

	(GLMBrick new
		hSpaceFill;
		vSpaceFill;
		addBrickBack: (
			self gtExample
				padding: 2;
				borderColor: Color lightGray;
				borderWidth: 2;
				margin: 10;
				hSpaceFill;
				vShrinkWrap;
				yourself);
		yourself)
			openInBrickWindow
			extent: 600@400