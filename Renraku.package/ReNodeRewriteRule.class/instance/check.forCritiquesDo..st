running
check: aNode forCritiquesDo: aCriticBlock

	matches keysAndValuesDo: [ :matcher :rewriter |
		matcher
			match: aNode
			onSuccess: [ :map |
				(self afterCheck: aNode mappings: map) ifTrue: [
					aCriticBlock cull: (
						self critiqueFor: aNode by: rewriter withMappings: map). ] ]
			onFailure: [  ] ]