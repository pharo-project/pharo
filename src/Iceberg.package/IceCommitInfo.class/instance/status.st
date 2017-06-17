*Iceberg-UI
status
	self isCurrent ifTrue: [ ^ 'Curent' ].
	self isLoaded ifTrue: [ ^ 'Loaded' ].
	self isMerged
		ifTrue: [ ^ 'Not loaded' ]
		ifFalse: [ ^ 'Not merged' ]
