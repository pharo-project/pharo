*GT-SpotterExtensions-Core
spotterHistoryFor: aStep
	<spotterOrder: 8>
	aStep listProcessor
			allCandidates: [ self history asArray select: [ :each | each isNotNil and: [ each value isNotNil ] ] ];
			title: 'History';
			itemName: [ :item | 
				item class = GTSpotterProcessorLink 
					ifTrue: [ item processor itemNameFor: item value ] " HACK: this triggers a recursion ! ... but processor is undefined for aCandidate "
					ifFalse: [ item gtDisplayString ] ];
			filter: GTFilterSubstring