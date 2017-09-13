hooks
afterCheck: aNode mappings: mappingDict
		
	aNode methodNode ifNotNil: [ :methNode |
		methNode compiledMethod ifNotNil: [ :method |
			(method selector = #initialize and: [ 
			method methodClass isMeta ])
				ifTrue: [ ^ true ] ] ].
		
	^ false
	