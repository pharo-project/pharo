examples - filtering
gtExampleCustomImplementorsSearchWithNavigationOn: aSpotter
	<gtExample>
	<label: 'Custom implementors search with navigation'>
	<description: 'A spotter for searching through the implementors of a method that 
	overrides properties of the search step and enables the Collection 
	processor for items (to allow dive in).'>
	<dependsOn: #gtExampleOnCompliedMethod>
	
	^ aSpotter
		processorsFilter: (GTSpotterMethodListProcessorFilter new 
			allowProcessorsFrom: Collection>>#spotterItemsFor:;
			allowProcessorsFrom: CompiledMethod>>#spotterForImplementorsFor: 
			configuredWith: [ :aProcessor | aProcessor
				actLogic: [ :aMethod :step | 
					step exit.
					aMethod inform: aMethod name ];
				candidatesLimit: 50 ])