protocol
protocolsOfSelector: aSelector

	^ (self protocols select: [:each | each includesSelector: aSelector ]) asArray