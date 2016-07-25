*GT-SpotterExtensions-Core
spotterForMessagesFor: aStep
	<spotterOrder: 35>

	aStep listProcessor
		title: 'Messages';
		filter: GTNullFilter item: [ :filter :context | 
			GTSelector substring: context textTrimmed filter: filter ];
		actLogic: [ :each :step|
			step exit.
			self systemNavigation browseAllImplementorsOf: each name ];		
		wantsToDisplayOnEmptyQuery: false