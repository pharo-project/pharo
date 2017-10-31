hooks
afterCheck: aNode mappings: mappingDict
		
	aNode methodNode ifNotNil: [ :methNode |
		methNode compiledMethod ifNotNil: [ :method |
			method methodClass withAllSubclassesDo: [ :class |
			 (class includesSelector: aNode selector)
				ifTrue: [ ^ false ] ]] ].
	^ true