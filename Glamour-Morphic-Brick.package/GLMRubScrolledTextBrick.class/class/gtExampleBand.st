examples
gtExampleBand
	<gtExample>
	<label: 'Two rubric text areas in band'>
	
	^ GLMBandBrick new
		padding: #(20 20);
		hSpaceFill;
		color: Color veryVeryLightGray;
		addAllBricks: { self new vShrinkWrap. self new vShrinkWrap};
		yourself