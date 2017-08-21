intializing
initializeWithBase: baseSnapshot target: targetSnapshot
	| base target |	
	operations := OrderedCollection new.
	base := MCDefinitionIndex definitions: baseSnapshot definitions.
	target := MCDefinitionIndex definitions: targetSnapshot definitions.
	
	target definitions 
		do: [ :t |
			base
				definitionLike: t
				ifPresent: [:b | (b isSameRevisionAs: t) ifFalse: [operations add: (MCModification of: b to: t)]]
				ifAbsent:  [operations add: (MCAddition of: t)] ]
		displayingProgress: 'Diffing...'.
		
	base definitions do: [:b |
		target
			definitionLike: b
			ifPresent: [:t |]
			ifAbsent: [operations add: (MCRemoval of: b)]]		