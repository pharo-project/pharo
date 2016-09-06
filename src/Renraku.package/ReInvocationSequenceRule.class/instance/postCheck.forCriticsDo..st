running
postCheck: anAST forCriticsDo: aCriticBlock
	| remainingPostConditions encounteredCritics |	
	postConditions ifEmpty: [ ^ false ].
	encounteredCritics := false.
	
	remainingPostConditions := postConditions copy.
	
	anAST reverseNodesDo: [ :node |
		
		remainingPostConditions ifEmpty: [ ^ encounteredCritics ].
		node isMessage ifTrue: [ 
			
			"remove the matched conditions while criticizing before-matches"
			remainingPostConditions removeAllSuchThat: [ :cond |
				| beforeMatched |
				(beforeMatched := cond beforeMatches: node selector) ifTrue: [ 
					
					encounteredCritics := true.
					aCriticBlock cull: (
						self postCriticFor: anAST at: node regarding: cond) ].
				 "if the following expression is true, the condition will be removed"
				beforeMatched or: [ cond afterMatches: node selector] ] ] ].
					

	^ encounteredCritics