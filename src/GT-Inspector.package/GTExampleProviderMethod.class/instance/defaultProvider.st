accessing-defaults
defaultProvider
	^ [ (Smalltalk classNamed: providerName) gtExamplesProvider ]
		on: Error
		do: [ nil ]