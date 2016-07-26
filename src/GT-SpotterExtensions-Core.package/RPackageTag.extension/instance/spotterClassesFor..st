*GT-SpotterExtensions-Core
spotterClassesFor: aStep
	<spotterOrder: 20>
	aStep listProcessor
			allCandidates: [ self classes sorted: [ :a :b | a name < b name ] ];
			title: 'Classes';
			filter: GTFilterSubstring