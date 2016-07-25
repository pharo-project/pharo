comparing
= other
	(self name = other name) ifFalse: [ ^false ].
	(self version = other version) ifFalse: [ ^false ].
	(self configurationClass  = other configurationClass) ifFalse: [ ^false ].
	(self repository = other repository) ifFalse: [ ^false ].
	^true