*renraku
critiques
	<eProperty>
	| rules critiques manifest builder |
	
	builder := TheManifestBuilder new.
	manifest := builder manifestOf: self.
		
	rules := ReRuleManager uniqueInstance classRules.
	critiques := Set new.
	rules
		reject: [ :rule |
			manifest isNotNil and:
			[ (builder bansRule: rule for: self) or:
			[  builder bansRule: rule for: self package ] ] ]
		thenDo: [ :rule |
			[ rule resetResult.
			  rule
				  check: self
				  forCritiquesDo: [ :critic |
					  critiques add: critic ]
			] on: Error
			  do: [ :er | ReSettings showErrors ifTrue: [ er pass ] ].
			Processor yield ].
	
	ReSystemAnnouncer uniqueInstance notifyEntity: self criticizedWith: critiques.
	
	^ critiques