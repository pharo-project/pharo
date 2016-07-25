external projects
fuelMetalevel: spec

	"Specify a dependency on an external project which has it's own configuration.
	The given project name can be chosen freely, for simplicity use the same name as the configuration or the conditional group you load."
	spec project: 'FuelMetalevel' with: [
		spec 
			repository: 'http://ss3.gemstone.com/ss/Fuel';
			className: 'ConfigurationOfFuel';
			"if you want to load by default a special group usse the #loads: message plus a group name of the external configuration"
			loads: #FuelMetalevel ].