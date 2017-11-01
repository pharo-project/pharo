Extension { #name : #Dictionary }

{ #category : #'*GT-SpotterExtensions-Core' }
Dictionary >> spotterForKeysFor: aStep [
	<spotterOrder: 15>
	aStep listProcessor
			title: 'Keys';
			allCandidates: [ self keys ];
			candidatesLimit: 5;
			filter: GTFilterSubstring
]
