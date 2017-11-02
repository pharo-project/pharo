Extension { #name : #KMDispatcher }

{ #category : #'*GT-SpotterExtensions-Core' }
KMDispatcher >> spotterForKeysFor: aStep [
	<spotterOrder: 15>
		
	aStep listProcessor
			title: 'Keys';
			allCandidates: [ (self gtAllKeyCategoryPairs collect: #key) asOrderedCollection ];
			itemName: [ :aKeymap | 
				aKeymap shortcut gtDisplayString, 
				(aKeymap name ifNil: [ '' ] ifNotNil: [ ' [', aKeymap name,']' ]) ];
			candidatesLimit: 5;
			filter: GTFilterSubstrings
]
