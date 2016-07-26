*GT-SpotterExtensions-Core
spotterSuperMethodsFor: aStep
	<spotterOrder: 11>
	aStep listProcessor
			title: 'Super instance methods';
			allCandidates: [ self allMethods reject: [ :each | each methodClass = self ] ];
			filter: GTFilterSubstring