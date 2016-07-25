ui
gtSpotterSubjectsFor: aStep
	<spotterOrder: 102>
	self subjects ifEmpty: [ ^ self ].
	aStep listProcessor
		title: 'Example Subjects';
		allCandidates: [ self subjects ];
		itemName: [ :subject | subject gtDisplayString ];
		filter: GTFilterSubstring