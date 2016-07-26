ui
gtSpotterProviderFor: aStep
	<spotterOrder: 103>
	self provider ifNil: [ ^ self ].
	aStep listProcessor
		title: 'Example Provider';
		items: [ Array with: self provider ];
		itemName: [ :aProvider | aProvider gtDisplayString ];
		filter: GTFilterSubstring