ui
spotterUsersFor: aStep
	<spotterOrder: 50>
	^ aStep listProcessor
		title: 'Senders';
		filter: GTFilterSubstring item: [ :filter :context | 
			self senders do: [ :sender | 
				filter value: sender compiledMethod ] ];
		keyBinding: $n meta