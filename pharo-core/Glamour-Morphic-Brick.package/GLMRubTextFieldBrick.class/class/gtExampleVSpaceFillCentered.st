examples
gtExampleVSpaceFillCentered
	<gtExample>
	<label: 'Rubric centered text field that shrinks to fit width of the text and fills parent height'>
	
	^ self gtExampleWithBorder
			vSpaceFill;
			beCentered;
			setTextWith: 'Hello world!';
			yourself