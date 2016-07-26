examples
exampleCollection

	GLMFoundationBrick new
		brickRoot: (self new model: (GLMBrickListModel new on: Smalltalk allClasses));
		extent: 300@400;
		openInWindow