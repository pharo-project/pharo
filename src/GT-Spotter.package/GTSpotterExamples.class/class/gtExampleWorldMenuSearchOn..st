examples - filtering
gtExampleWorldMenuSearchOn: aSpotter
	<gtExample>
	<label: 'World menu search'>
	<description: 'A spotter for searching through the world menu'>
	<dependsOn: #gtExampleBasic>

	^ aSpotter
		filterByMethods: {
			GTSpotter>>#spotterWorldMenuFor:}
	
	