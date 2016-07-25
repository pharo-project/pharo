actions
convertValues
	existing 
		at: #computerUUID 
		ifPresent: [ :aValue | existing at: #computerUUID put: aValue asString ]
		ifAbsent: [ "ignore" ].
	existing 
		at: #secretUUID
		ifPresent: [ :aValue | existing at: #secretUUID put: aValue asString ]
		ifAbsent: [ "ignore" ].