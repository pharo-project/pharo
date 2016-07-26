running
preCheck: anAST forCriticsDo: aCriticBlock
	| remainingPreConditions encounteredCritics |	
		
	preConditions ifEmpty: [ ^ false ].
	encounteredCritics := false.
	
	remainingPreConditions := preConditions copy.
	
	anAST nodesDo: [ :node |
		remainingPreConditions ifEmpty: [ ^ encounteredCritics ].
		node isMessage ifTrue: [ 
			remainingPreConditions do: [ :cond | | afterMatched |
				
				(afterMatched := cond afterMatches: node selector) ifTrue: [ 
					encounteredCritics := true.
					aCriticBlock cull: (
						self preCriticFor: anAST at: node regarding: cond) ].
				 
				(afterMatched or: [ cond beforeMatches: node selector]) ifTrue: [
					remainingPreConditions remove: cond ] ] ] ].
					

	^ encounteredCritics