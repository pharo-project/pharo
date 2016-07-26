methods
collectionProtocol
	| newCollection |
	newCollection := OrderedCollection new.
	(1 to: 10) asOrderedCollection do: 
			[:each | 
			| new |
			new := each * 2.
			newCollection add: new].
	^newCollection