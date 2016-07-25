*GT-SpotterExtensions-Core
spotterForSubmorphsFor: aStep
	<spotterOrder: 10>
	self submorphs ifNil: [ ^ self ].
	
	aStep listProcessor
			title: 'Submorphs';
			allCandidates: [ self submorphs ];
			candidatesLimit: 5;
			filter: GTFilterSubstring