comparing
= other
	(super = other) ifFalse: [ ^false ].
	(self configurationClass = other configurationClass) ifFalse: [ ^false ].
	(self displayName = other displayName) ifFalse: [ ^false ].
	(self repositories hasEqualElements: other repositories) ifFalse: [ ^false ].
	^ true