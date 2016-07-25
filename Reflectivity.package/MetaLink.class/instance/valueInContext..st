ast
valueInContext: aBlock
	(RFMetaContext current isActive: (self level ifNil: [0]))
		ifFalse: [ ^ self ].
	RFMetaContext current shiftLevelUp.
	^ aBlock ctxtEnsure: [ RFMetaContext current shiftLevelDown ]