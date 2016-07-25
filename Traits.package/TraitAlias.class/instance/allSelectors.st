enquiries
allSelectors
	^self subject allSelectors
		addAll: (self aliases collect: [:each | each key]) asSet;
		yourself