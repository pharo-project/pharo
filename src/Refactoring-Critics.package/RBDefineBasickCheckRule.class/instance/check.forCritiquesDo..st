as yet unclassified
check: aClass forCritiquesDo: aCritiqueBlock
	((aClass inheritsFrom: RBLintRule) and: [ 
		aClass isVisible and: [ 
	  (aClass lookupSelector: #basicCheck:) isSubclassResponsibility ] ])

		ifTrue: [ aCritiqueBlock cull: (
			ReMissingMethodCritique
				for: aClass
				by: self
				class: aClass
				selector: #basicCheck:)
				beShouldBeImplemented ]