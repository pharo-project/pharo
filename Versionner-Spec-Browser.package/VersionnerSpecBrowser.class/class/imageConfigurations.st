configurations
imageConfigurations
	| configs |
	
	configs := MBConfigurationRoot current configurations.
	configs := configs reject: [ :config | self testConfigurations includes: config name ].
	^ configs