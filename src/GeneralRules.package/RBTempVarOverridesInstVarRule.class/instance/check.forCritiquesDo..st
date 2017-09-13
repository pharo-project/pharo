running
check: aMethod forCritiquesDo: aCriticBlock
	| ivarNames |
	
	ivarNames := aMethod methodClass instVarNames.
	ivarNames ifEmpty: [ ^ self ].
	
	(aMethod ast arguments, aMethod ast temporaries)
		do: [ :node |
			(ivarNames includes: node name)
				ifTrue: [ 
					aCriticBlock cull: (
						self
							critiqueFor: aMethod
							about: node) ] ]
		
