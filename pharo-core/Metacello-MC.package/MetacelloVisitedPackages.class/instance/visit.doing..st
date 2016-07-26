visiting
visit: aSpec doing: aBlock

	aSpec
		projectDo: [:spec | 
			(projects includes: spec name) ifTrue: [ ^self ].
			projects add: spec name ] 
		packageDo: [:spec | 
			(packages includes: spec name) ifTrue: [ ^self ].
			packages add: spec name ] 
		groupDo: [:spec | 
			(groups includes: spec name) ifTrue: [ ^self ].
			groups add: spec name ].
	aBlock value: aSpec