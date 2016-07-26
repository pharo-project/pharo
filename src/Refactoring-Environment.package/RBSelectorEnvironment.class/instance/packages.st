accessing
packages
	"Check that packages have really class and selector included."

	| pSet |
	pSet := Set new.
	self classes
		do: [ :each | 
			each packages
				do: [ :p | 
					self
						selectorsForClass: each
						do: [ :s | 
							(p includesSelector: s ofClass: each)
								ifTrue: [ pSet add: p ] ] ] ].
	^ pSet