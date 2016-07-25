actions
addDefinitionsTo: aCollection
	| definition |
	definition := aCollection 
					detect: [:ea | ea isOrganizationDefinition ] 
					ifNone: [aCollection add: (MCOrganizationDefinition categories: #())].
	definition categories: (definition categories copyWith: self category).