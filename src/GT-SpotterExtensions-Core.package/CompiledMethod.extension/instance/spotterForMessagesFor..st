*GT-SpotterExtensions-Core
spotterForMessagesFor: aStep
	<spotterOrder: 12>
	aStep listProcessor
			title: 'Messages';
			allCandidates: [ self messages asSortedCollection collect: [:each | GTSelector new name: each ] ];
			filter: GTFilterSubstring