testing
includesKey: aKey
	^ self at: aKey ifPresent: [ :value | true ] ifAbsent: [ false ].