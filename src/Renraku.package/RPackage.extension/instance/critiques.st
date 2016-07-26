*renraku
critiques
	<eProperty>
	| rules critiques manifest builder |
	
	builder := TheManifestBuilder new.
	manifest := builder manifestOf: self.
		
	rules := ReRuleManager uniqueInstance packageRules.
	critiques := Set new.
	rules
		reject: [ :rule |
			manifest isNotNil and:
			[ builder bansRule: rule for: self ] ]
		thenDo: [ :rule |
			[ rule resetResult.
			  rule
				  check: self
				  forCritiquesDo: [ :critique |
					  critiques add: critique ]
			] on: Error
			  do: [ :er | ReSettings showErrors ifTrue: [ er pass ] ].
			Processor yield ].
	
	ReSystemAnnouncer uniqueInstance notifyEntity: self criticizedWith: critiques. .
	
	^ critiques