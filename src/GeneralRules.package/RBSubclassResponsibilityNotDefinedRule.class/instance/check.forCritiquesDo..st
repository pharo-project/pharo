running
check: aClass forCritiquesDo: aCriticBlock

	(aClass whichSelectorsReferTo: #subclassResponsibility) do:  
		[ :selector | 
		(aClass subclasses do: [ :class | 
				(class subclasses isEmpty and:
				[ (class whichClassIncludesSelector: selector) == aClass ]) ifTrue: [ 
					aCriticBlock cull: (ReMissingMethodCritique
						for: aClass
						by: self
						class: class
						selector: selector)
						beShouldBeImplemented;
						yourself ] ]) ].
	