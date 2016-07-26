*renraku
check: aMethod forCritiquesDo: aCriticBlock
	
	(self basicCheck: aMethod)
		ifTrue: [
			aCriticBlock cull: (
				ReTransformationCritique
					for: aMethod
					by: self
					tree: rewriteRule tree)  ]
		