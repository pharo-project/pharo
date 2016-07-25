system changes
managersForPackage: aPackage do: aBlock
	self registry do: [:mgr | 
		(mgr packageSet includesSystemCategory: aPackage name) ifTrue: [
			aBlock value: mgr.
		]
	].