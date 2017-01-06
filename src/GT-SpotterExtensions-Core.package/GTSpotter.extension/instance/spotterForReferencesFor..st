*GT-SpotterExtensions-Core
spotterForReferencesFor: aStep
	<spotterOrder: 32>
	
	aStep listProcessor
		title: 'References';
		filter: GTNullFilter item: [ :filter :context |
			self class environment at: context textTrimmed ifPresent: [ :class |  
				SystemNavigation default allReferencesTo: class binding do: filter ] ];
		keyBinding: $n shift meta;
		itemName: [ :method | method gtDisplayString ]