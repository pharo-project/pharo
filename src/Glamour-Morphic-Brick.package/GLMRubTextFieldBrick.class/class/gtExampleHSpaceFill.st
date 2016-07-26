examples
gtExampleHSpaceFill
	<gtExample>
	<label: 'Rubric text field that shrinks to fit height of the text and fills parent width'>
	
	^ self gtExampleWithBorder
			hSpaceFill;
			setTextWith: 'Hello world!';
			yourself