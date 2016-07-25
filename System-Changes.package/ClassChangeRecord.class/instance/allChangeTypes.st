all changes
allChangeTypes
	| chgs |
	(priorName notNil and: [ changeTypes includes: #rename ])
		ifTrue: [ 
			(chgs := changeTypes copy) add: 'oldName: ' , priorName.
			^ chgs ].
	^ changeTypes