as yet unclassified
check: node forCritiquesDo: aCriticBlock
	(node isMessage
		and: [ node isUsed not
				and: [ #(#= #== #~= #~~ #< #> #<= #>=) includes: node selector ] ])
		ifTrue: [ aCriticBlock cull: (self critiqueFor: node) ]