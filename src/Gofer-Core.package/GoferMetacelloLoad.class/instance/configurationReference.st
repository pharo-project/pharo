accessing
configurationReference
	^ gofer references
		detect: [ :ref | ref isConfigurationReference ]
		ifNone: [ 
			gofer configuration.
			^ self configurationReference ].