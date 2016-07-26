accessing
reasonCodeDescriptions

	reasonCodeDescriptions ifNil: [ reasonCodeDescriptions := self populateReasonCodeDescriptions ].
	^reasonCodeDescriptions