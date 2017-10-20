running
check: aNode forCritiquesDo: aCriticBlock
	
	matchers do: [ :matcher |
		matcher
			match: aNode
			onSuccess: [ :map |
				(self
					afterCheck: aNode
					mappings: (self flattenMatchMap: map))
						ifTrue: [
							aCriticBlock cull: (self critiqueFor: aNode) ] ]
			onFailure: [  ] ]
	