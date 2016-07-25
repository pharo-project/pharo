examples
gtExampleScrollPane
	<gtExample>
	<label: 'Multiple rubric text areas in scroll pane'>
	
	^ GLMScrollPaneBrick new
		hSpaceFill;
		vSpaceFill;
		color: Color veryVeryLightGray;
		layoutMorphs: true;
		width: 350;
		height: 200;
		addAllBricks: ((1 to: 20) collect: [ :i | self gtExampleBasic ]);
		yourself