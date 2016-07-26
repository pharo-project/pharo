examples
gtExample
	<gtExample>
	<label: 'Two rubric text areas filling half of size'>
	
	^ GLMBrick new
		useVerticalLinearLayout;
		padding: #(20 20);
		hSpaceFill;
		vSpaceFill;
		color: Color veryVeryLightGray;
		addAllBricks: { self new vSpaceFill: 50. self new vSpaceFill: 50 };
		yourself