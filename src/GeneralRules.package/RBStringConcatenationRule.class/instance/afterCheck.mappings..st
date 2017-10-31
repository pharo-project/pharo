hooks
afterCheck: aNode mappings: mappingDict
	(self performsConcatenation: (mappingDict at: '``@argument'))
		ifTrue: [ ^ true ].
		
	(mappingDict
		at: '``@argument2'
		ifPresent: [ :arg2 |
			^ self performsConcatenation: arg2 ]).
		
	^ false 