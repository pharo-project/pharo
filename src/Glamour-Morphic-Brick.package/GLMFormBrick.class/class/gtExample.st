examples
gtExample
	<gtExample>
	<label: 'Input form with title, two labels, four checkboxes, text and submit button'>
	
	^ self new
		title: [ :t | t
			text: 'Grüezi' ];
		text: [ :t | t
			text: 'Welcome to the Restaurant Zukunft zu Pharo in Berne'];
		label: [ :l | l
			text: 'What would you like to eat?' ];
		checkbox: [ :c | c
			id: #fondue;
			check;
			text: 'Cheese Fondue, CHF 20.-' ];
		checkbox: [ :c | c
			id: #raclette;
			text: 'Raclette, CHF 15.-' ];
		
		label: [ :l | l 
			text: 'What would you like to drink?' ];
		checkbox: [ :c | c
			id: #schorle;
			text: 'Apfel Schorle, CHF 4.-' ];
		checkbox: [ :c | c
			id: #rivella;
			check;
			text: 'Rivella, CHF 4.-' ];
		submit;
		yourself