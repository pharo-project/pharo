examples
gtExampleFullSpaceFill
	<gtExample>
	<label: 'Rubric centered text field that fills parent'>
	
	^ self gtExampleWithBorder
			hSpaceFill;
			vSpaceFill;
			setTextWith: 'Hello world!';
			yourself