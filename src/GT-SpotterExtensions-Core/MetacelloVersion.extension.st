Extension { #name : #MetacelloVersion }

{ #category : #'*GT-SpotterExtensions-Core' }
MetacelloVersion >> spotterForGroupsFor: aStep [
	<spotterOrder: 2>
	aStep listProcessor
		title: 'Groups';
		allCandidates: [ self groups ];
		itemName: [ :item | item name ];
		filter: GTFilterSubstring;
		wantsToDisplayOnEmptyQuery: true
]

{ #category : #'*GT-SpotterExtensions-Core' }
MetacelloVersion >> spotterForPackagesFor: aStep [
	<spotterOrder: 3>
	aStep listProcessor
		title: 'Packages';
		allCandidates: [ self packages ];
		itemName: [ :item | item file ];
		filter: GTFilterSubstring;
		wantsToDisplayOnEmptyQuery: true
]
