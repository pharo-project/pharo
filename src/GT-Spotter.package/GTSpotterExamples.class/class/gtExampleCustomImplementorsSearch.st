examples - filtering
gtExampleCustomImplementorsSearch
	<gtExample>
	<label: 'Custom implementors search'>
	<description: 'A spotter for searching through the implementors of a 
	method that overrides properties of the search step.'>
	| spotter |
	spotter := self gtExampleOnCompliedMethod.	
	^ spotter
		processorsFilter: (GTSpotterMethodListProcessorFilter new 
			allowProcessorsFrom: CompiledMethod>>#spotterForImplementorsFor: 
			configuredWith: [ :aProcessor | aProcessor
				actLogic: [ :aMethod :step | 
					step exit.
					aMethod inform: aMethod name ];
				candidatesLimit: 50 ])