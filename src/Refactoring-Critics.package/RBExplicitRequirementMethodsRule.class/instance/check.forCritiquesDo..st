running
check: aClass forCritiquesDo: aCritiqueBlock

	| explicitRequirementMethods |
	
	
	aClass isTrait 
		ifTrue: [ ^ self ].
		
	explicitRequirementMethods := aClass traitComposition allSelectors
		collect: [ :selector |
			aClass>>selector ]
		thenSelect: [ :method |
			method isRequired and: [ method isSubclassResponsibility not ] ].
	
	explicitRequirementMethods
		select: [ :method |
      		aClass withAllSuperclasses noneSatisfy: [ :superclass |
				superclass canPerform: method selector ] ]
		thenDo: [ :method |
			aCritiqueBlock cull: (
				(self critiqueFor: aClass)
  					tinyHint: method selector;
  					yourself) ]