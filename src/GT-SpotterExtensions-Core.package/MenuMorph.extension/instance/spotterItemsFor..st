*GT-SpotterExtensions-Core
spotterItemsFor: aStep
	<spotterOrder: 10>
	self flag: #refactoringViolationForSpotterActDefault:.
	^ aStep listProcessor 
		title: 'Items';
		candidatesLimit: Float infinity;
		allCandidates: [ self items ];
		itemName: #contents;
		itemIcon: [ :item | item icon ];
		actLogic: [ :item :step | 
			item hasSubMenu 
				ifTrue: [ step diveIn ] 
				ifFalse: [ item spotterActDefault. step exit ] ];
		filter: GTFilterSubstring 