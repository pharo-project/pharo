*renraku
check: aClass forCritiquesDo: aCriticBlock
	(self basicCheck: aClass)
		ifTrue: [ aCriticBlock cull: (
			ReMissingMethodCritique
				for: aClass
				by: self
				class: aClass
				selector: #basicCheck:)
				beShouldBeImplemented ]