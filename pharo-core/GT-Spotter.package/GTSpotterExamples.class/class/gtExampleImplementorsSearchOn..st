examples - filtering
gtExampleImplementorsSearchOn: aSpotter
	<gtExample>
	<label: 'Implementors search on single method'>
	<description: 'A spotter for searching through the implementors of a method.'>
	<depends: #gtExampleOnCompliedMethod>
	
	^ aSpotter
		filterByMethods: {
			CompiledMethod>>#spotterForImplementorsFor:}