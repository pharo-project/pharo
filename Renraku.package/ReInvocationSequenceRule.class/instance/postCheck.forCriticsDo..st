running
postCheck: anAST forCriticsDo: aCriticBlock
	| remainingPostConditions encounteredCritics |	
	postConditions ifEmpty: [ ^ false ].
	encounteredCritics := false.
	
	remainingPostConditions := postConditions copy.
	
	anAST reverseNodesDo: [ :node |
		
		remainingPostConditions ifEmpty: [ ^ encounteredCritics ].
		node isMessage ifTrue: [ 
			remainingPostConditions do: [ :cond | | beforeMatched |

				(beforeMatched := cond beforeMatches: node selector) ifTrue: [ 
					
					encounteredCritics := true.
					aCriticBlock cull: (
						self postCriticFor: anAST at: node regarding: cond) ].
				 
				(beforeMatched or: [ cond afterMatches: node selector]) ifTrue: [
					remainingPostConditions remove: cond ] ] ] ].
					

	^ encounteredCritics