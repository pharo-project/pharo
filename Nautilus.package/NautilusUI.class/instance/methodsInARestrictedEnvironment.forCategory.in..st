widget method
methodsInARestrictedEnvironment: env forCategory: aCategory  in: class
	| mthds selectors |
	
	aCategory = self allLabel
		ifTrue: [ ^ class methods asOrderedCollection select: [:e | env includesSelector: e selector in: class ]].
	selectors := env selectorsFor: aCategory in: class.
	mthds := OrderedCollection new.
	selectors do: [:s | mthds add: (class compiledMethodAt: s) ].
	^ mthds