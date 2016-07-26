*GT-SpotterExtensions-Core
spotterUsedSlotsFor: aStep
	<spotterOrder: 70>
	aStep listProcessor
			title: 'Special Slots';
			allCandidates: [ self slots select: [ :slot | slot isSpecial ] ];
			itemName: [ :item | item definitionString ];
			filter: GTFilterSubstring