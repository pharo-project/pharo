system changes
managersForClass: aClass do: aBlock

	self registry do: [:mgr |
		(mgr packageSet includesClass: aClass)
			ifTrue: [aBlock value: mgr]]