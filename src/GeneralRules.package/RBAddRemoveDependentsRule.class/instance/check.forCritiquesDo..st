running
check: aClass forCritiquesDo: aCriticBlock
	| addSends removeSends |
	addSends := 0.
	removeSends := 0.
	(aClass whichSelectorsReferTo: #addDependent:) asSet
		addAll: (aClass whichSelectorsReferTo: #removeDependent:);
		do: [ :sel | 
			(aClass compiledMethodAt: sel) ast
				nodesDo: [ :each | 
					each isMessage
						ifTrue: [
							each selector = #addDependent:
								ifTrue: [ addSends := addSends + 1 ].
							each selector = #removeDependent:
								ifTrue: [ removeSends := removeSends + 1 ] ] ] ].
	addSends > removeSends ifTrue: [ aCriticBlock cull: (self critiqueFor: aClass) ]