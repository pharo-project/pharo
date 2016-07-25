running
basicCheck: anEntity

	^ anEntity package ifNil: [ true ] ifNotNil: #isDefault