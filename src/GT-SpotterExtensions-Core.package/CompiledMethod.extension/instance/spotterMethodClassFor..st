*GT-SpotterExtensions-Core
spotterMethodClassFor: aStep
	<spotterOrder: 50>
	aStep listProcessor
			allCandidates: [  {self methodClass }];
			title: 'Class';
			filter: GTFilterSubstring;
			itemIcon: #systemIcon