*GT-SpotterExtensions-Core
spotterForReferencesFor: aStep
	<spotterOrder: 32>
	
	aStep listProcessor
		title: 'References' translated;
		filter: GTNullFilter item: [ :filter :context |
			self class environment at: context textTrimmed asSymbol ifPresent: [ :class | 
				SystemNavigation default allReferencesTo: class binding do: filter ] ];
		keyBinding: $n shift meta;
		itemName: [ :method | method gtDisplayString ]