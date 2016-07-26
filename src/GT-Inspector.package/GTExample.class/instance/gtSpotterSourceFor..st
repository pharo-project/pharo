ui
gtSpotterSourceFor: aStep
	<spotterOrder: 103>
	self source ifNil: [ ^ self ].
	aStep listProcessor
		title: 'Example Source';
		items: [ Array with: self source ];
		itemName: [ :aSource | aSource gtDisplayString ];
		filter: GTFilterSubstring