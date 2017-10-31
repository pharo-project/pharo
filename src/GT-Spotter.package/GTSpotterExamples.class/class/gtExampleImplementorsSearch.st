examples - filtering
gtExampleImplementorsSearch
	<gtExample>
	<label: 'Implementors search on single method'>
	<description: 'A spotter for searching through the implementors of a method.'>
	| spotter |
	spotter := self gtExampleOnCompliedMethod.
	^ spotter
		filterByMethods: {
			CompiledMethod>>#spotterForImplementorsFor:}