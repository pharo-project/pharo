examples
gtExampleFullSpaceFillCentered
	<gtExample>
	<label: 'Rubric centered text field that fills parent'>
	
	^ self gtExampleWithBorder
			hSpaceFill;
			vSpaceFill;
			beCentered;
			setTextWith: 'Hello world!';
			yourself