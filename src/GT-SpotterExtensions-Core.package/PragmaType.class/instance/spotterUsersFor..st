ui
spotterUsersFor: aStep
	<spotterOrder: 50>
	^ aStep listProcessor
		title: 'Users';
		filter: GTFilterSubstring item: [ :filter :context | 
			self users do: [ :sender | filter value: sender ] ]