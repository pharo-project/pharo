*GT-SpotterExtensions-Core
spotterForReferencesFor: aStep
	<spotterOrder: 130>
	aStep listProcessor
			title: 'References';
			allCandidates: [ (SystemNavigation default allReferencesTo: self) collect: #compiledMethod ];
			itemName: [ :method | method gtDisplayString ];
			filter: GTFilterSubstring