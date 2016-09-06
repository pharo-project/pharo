running
preCheck: anAST forCriticsDo: aCriticBlock
	| remainingPreConditions encounteredCritics |	
		
	preConditions ifEmpty: [ ^ false ].
	encounteredCritics := false.
	
	remainingPreConditions := preConditions copy.
	
	anAST nodesDo: [ :node |
		remainingPreConditions ifEmpty: [ ^ encounteredCritics ].
		node isMessage ifTrue: [ 
			
			"remove the matched conditions while criticizing after-matches"
			remainingPreConditions removeAllSuchThat:  [ :cond |
				| afterMatched |
				(afterMatched := cond afterMatches: node selector) ifTrue: [ 
					encounteredCritics := true.
					aCriticBlock cull: (
						self preCriticFor: anAST at: node regarding: cond) ].
				 
				"if the following expression is true, the condition will be removed"
				afterMatched or: [ cond beforeMatches: node selector] ] ] ].
					

	^ encounteredCritics