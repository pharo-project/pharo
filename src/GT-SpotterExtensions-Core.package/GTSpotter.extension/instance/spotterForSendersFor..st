*GT-SpotterExtensions-Core
spotterForSendersFor: aStep
	<spotterOrder: 31>

	aStep listProcessor
		title: 'Senders';
		filter: GTNullFilter item: [ :filter :context | 
			SystemNavigation default
				allSendersToString: context textTrimmed 
				do: filter ];
		keyBinding: $n meta;
		wantsToDisplayOnEmptyQuery: false