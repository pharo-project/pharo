examples
gtExampleVSpaceFill
	<gtExample>
	<label: 'Rubric text field that shrinks to fit width of the text and fills parent height'>
	
	^ self gtExampleWithBorder
			vSpaceFill;
			setTextWith: 'Hello world!';
			yourself