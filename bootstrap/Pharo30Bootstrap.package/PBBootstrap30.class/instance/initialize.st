running
initialize

	super initialize.
	systemDefinition ifNil: [ 
		systemDefinition := (FiMetacelloWriter new written: 
			(ConfigurationOfPharoBootstrap project version: '3.864')) ].
	bootstrappedBindingCache := EPDictionary new.
	remoteMetaclassBindings := EPDictionary new.