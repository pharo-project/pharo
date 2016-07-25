examples - filtering
gtExampleWorldMenuSearchOn: aSpotter
	<gtExample>
	<label: 'World menu search'>
	<description: 'A spotter for searching through the world menu'>
	<depends: #gtExampleBasic>

	^ aSpotter
		filterByMethods: {
			GTSpotter>>#spotterWorldMenuFor:}
	
	