running
basicCheck: aNode

	matchers do: [ :matcher |
		matcher
			match: aNode
			onSuccess: [ :map |
				(self afterCheck: aNode mappings: map)
					ifTrue: [ ^ true ] ]
			onFailure: [  ] ].
		
	^ false
	