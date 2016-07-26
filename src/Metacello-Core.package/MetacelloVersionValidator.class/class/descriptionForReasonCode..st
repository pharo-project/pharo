accessing
descriptionForReasonCode: reasonCode
	^ self reasonCodeDescriptions at: reasonCode ifAbsent: [ self error: 'Unknown reasonCode: ' , reasonCode printString ]