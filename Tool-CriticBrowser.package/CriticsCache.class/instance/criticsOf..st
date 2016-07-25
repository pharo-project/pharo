protocol
criticsOf: aRule

	aRule ifNil: [ ^ {} ].	
	^ aRule isComposite
		ifTrue: [ aRule leaves flatCollect: [ :rule | self criticsOf: rule ]]
		ifFalse: [ critics at: aRule ifAbsent: [{}]]