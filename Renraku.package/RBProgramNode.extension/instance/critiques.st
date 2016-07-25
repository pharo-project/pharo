*renraku
critiques
	<eProperty>

	| rules critiques |
		
	rules    := ReRuleManager uniqueInstance nodeRules.
	critiques  := Set new.
		
	rules do: [ :rule |
				[
					rule
						check: self
				 		forCritiquesDo: [ :critic |
					  		critiques add: critic ]
				] on: Error
			  	  do: [ :er | ReSettings showErrors ifTrue: [ er pass ] ].
			Processor yield ].
	
	^ critiques