*GT-SpotterExtensions-Core
spotterClassesFor: aStep
	<spotterOrder: 20>
	aStep listProcessor
			allCandidates: [ self definedClasses sorted: [ :a :b | a name < b name ] ];
			title: 'Classes';
			filter: GTFilterSubstring