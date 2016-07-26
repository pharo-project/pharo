*GT-SpotterExtensions-Core
spotterForSendersFor: aStep
	<spotterOrder: 11>
	aStep listProcessor
			title: 'Senders';
			allCandidates: [ self senders collect: #compiledMethod ];
			filter: GTFilterSubstring;
			keyBinding: $n command