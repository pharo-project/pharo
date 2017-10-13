running
critiquesOf: anEntity
	| rules critiques manifest builder |
	
	builder := TheManifestBuilder new.
	manifest := builder manifestOf: anEntity.
		
	rules := anEntity suitableRulesFrom: (ReRuleManager managerFor: anEntity).
	critiques := Set new.
	rules
		reject: [ :rule |
			manifest isNotNil and:
			[ anEntity banChecksForValidation
				anySatisfy: [ :banLevel |
					builder bansRule: rule for: banLevel ] ] ]
		thenDo: [ :rule |
			[ rule resetResult.
			  rule
				  check: anEntity
				  forCritiquesDo: [ :critic |
					  critiques add: critic ]
			] on: Error
			  do: [ :er |
				ReExceptionStrategy current
					handle: er
					about: anEntity
					forProppertiesDo: [ :prop |
						critiques add: prop ] ].
			Processor yield ].
	
	ReSystemAnnouncer uniqueInstance notifyEntity: anEntity criticizedWith: critiques.
	
	^ critiques 