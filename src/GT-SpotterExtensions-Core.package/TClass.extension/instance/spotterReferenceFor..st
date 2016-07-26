*GT-SpotterExtensions-Core
spotterReferenceFor: aStep
	<spotterOrder: 30>
	aStep listProcessor
			title: 'References';
			allCandidates: [ (SystemNavigation default allReferencesTo: self binding) collect: #compiledMethod ];
			itemName: [ :method | method gtDisplayString ];
			filter: GTFilterSubstring