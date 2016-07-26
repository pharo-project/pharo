*GT-SpotterExtensions-Core
"protocol: *GT-SpotterExtensions-Core"
spotterAncestorsFor: aStep
	<spotterOrder: 1>
	self flag: 'rewrite for direct streaming - get rid of recursion'.
	aStep listProcessor
		title: 'Ancestors';
		allCandidates: [ self allAncestors ];
		filter: GTFilterSubstring