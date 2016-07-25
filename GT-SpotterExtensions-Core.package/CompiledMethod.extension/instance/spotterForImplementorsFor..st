*GT-SpotterExtensions-Core
spotterForImplementorsFor: aStep
	<spotterOrder: 10>
	aStep listProcessor
			title: 'Implementors';
			allCandidates: [ self implementors collect: #compiledMethod ];
			filter: GTFilterSubstring;
			keyBinding: $m command