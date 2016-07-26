examples
gtExampleWithLabel
	<gtExample>
	<label: 'Checkbox with label'>
	
	^ self new
		text: 'I''m label, click me';
		action: [ :boolean | self inform: 'Checkbox set to ', boolean asString ];
		yourself