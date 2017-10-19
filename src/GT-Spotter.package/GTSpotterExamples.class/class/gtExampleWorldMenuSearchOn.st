examples - filtering
gtExampleWorldMenuSearchOn
	<gtExample>
	<label: 'World menu search'>
	<description: 'A spotter for searching through the world menu'>
	| spotter |
	spotter := self gtExampleBasic.
	^ spotter
		filterByMethods: {
			GTSpotter>>#spotterWorldMenuFor:}
	
	