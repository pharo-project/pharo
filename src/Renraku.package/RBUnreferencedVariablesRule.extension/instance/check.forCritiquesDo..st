*renraku
check: aClass forCritiquesDo: aCriticBlock
	
	| allSubclasses encounteredCritic |
	encounteredCritic := false.
	allSubclasses := aClass withAllSubclasses.
	aClass instVarNames do: 
		[ :each | 
		(allSubclasses 
			noneSatisfy: [ :class | (class whichSelectorsAccess: each) isNotEmpty ])
			ifTrue: 
				[
					encounteredCritic := true.
					aCriticBlock cull: 
						(ReTrivialCritique
							withAnchor: (ReSearchStringSourceAnchor
								entity: aClass
								string: each)
							by: self
							hint: each) ] ].
				
	aClass isMeta ifFalse: 
		[ aClass classPool associationsDo: 
			[ :each | 
			(SystemNavigation default allCallsOn: each) ifEmpty: 
				[ encounteredCritic := true.
					aCriticBlock cull:
						(ReTrivialCritique
							withAnchor: (ReSearchStringSourceAnchor
								entity: aClass
								string: each key)
							by: self
							hint: each key) ] ] ].
			