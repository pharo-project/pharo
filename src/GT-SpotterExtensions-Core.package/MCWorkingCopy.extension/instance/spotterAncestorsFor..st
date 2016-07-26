*GT-SpotterExtensions-Core
spotterAncestorsFor: aStep
	<spotterOrder: 2>
	self flag: #maybeRewriteForDirectStreaming.
	aStep listProcessor
		title: 'Ancestors';
		allCandidates: [ self allAncestors ];
		filter: GTFilterSubstring