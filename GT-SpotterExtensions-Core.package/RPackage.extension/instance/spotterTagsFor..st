*GT-SpotterExtensions-Core
spotterTagsFor: aStep
	<spotterOrder: 20>
	self classTags size <= 1 ifTrue: [ ^ self ].
	aStep listProcessor
			allCandidates: [ self classTags sorted: [ :a :b | a name < b name ] ];
			title: 'Tags';
			itemName: [ :item | item name ];
			filter: GTFilterSubstring