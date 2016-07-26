transforming
inlineSelfSends
	class withAllSubclasses do: 
			[:each | 
			| selectors |
			selectors := each selectors.
			selectors remove: selector ifAbsent: [].
			selectors do: [:sel | self inlineMessagesInClass: each andSelector: sel]]