*renraku
nodeCritiques
	| rules critiques manifest builder |
	
		
	builder := TheManifestBuilder new.
	manifest := builder manifestOf: self.
		
	rules    := ReRuleManager uniqueInstance nodeRules.
	critiques  := Set new.
	rules := rules
		reject: [ :rule |
			manifest isNotNil and:
			[ (builder bansRule: rule for: self) or:
			[  builder bansRule: rule for: self methodClass ] ] ].
		
	self ast 	nodesDo: [ :node |
			rules do: [ :rule |
				[
					rule
						check: node
				 		forCritiquesDo: [ :critique |
							critique sourceAnchor initializeEnitity: self.
					  		critiques add: critique ]
				] on: Error
			  	  do: [ :er | ReSettings showErrors ifTrue: [ er pass ] ] ].
			Processor yield ].
	
	^ critiques