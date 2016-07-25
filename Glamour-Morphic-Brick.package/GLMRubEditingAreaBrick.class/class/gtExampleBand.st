examples
gtExampleBand
	<gtExample>
	<label: 'Two rubric text areas in band'>
	
	^ GLMBandBrick new
		padding: #(20 20);
		hSpaceFill;
		vShrinkWrap;
		width: 350;
		height: 200;
		color: Color veryVeryLightGray;
		addAllBricks: { self gtExampleBasic. self gtExampleBasic };
		yourself