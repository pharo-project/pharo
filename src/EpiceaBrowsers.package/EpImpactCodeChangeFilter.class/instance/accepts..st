filtering
accepts: anOmEntry

	anOmEntry content isCodeChange ifFalse: [ ^false ].

	^ anOmEntry content accept: (EpHasImpactVisitor inEnvironment: environment)