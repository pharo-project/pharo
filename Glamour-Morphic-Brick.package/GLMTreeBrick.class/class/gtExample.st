examples
gtExample
	<gtExample>
	<label: 'Simple tree with a few elements'>
	
	^ self new
		hSpaceFill;
		vShrinkWrap;
		on: {
			'First' -> '1'.
			'Second' -> '2'.
			'Third' -> { 
				'Sub First' -> '3-1'.
				'Sub Second' -> '3-2' } }
	