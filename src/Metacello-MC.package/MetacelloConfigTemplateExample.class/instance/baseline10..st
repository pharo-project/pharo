baselines
baseline10: spec
	"Baselines are used by convention in Metacello and essentially are nothing else than normal versions.
	Name the baseline after the first version it was introduced.
	In this case 1.0-baseline was introduced the first time with the 1.0 version defined in the #version10 method.
	Metacello only uses the following tag to figure out the name of this baseline:"
	<version: '1.0-baseline'>

	"Using #common makes this dependency declaration available for all Smalltalks.
	If you need more fine-grained control you can add several #for:do: sections for other releases."
	spec for: #common do: [	
		spec blessing: #baseline.
		"specify the default repository for your project's packages"
		spec repository: 'http://smalltalkhub.com/mc/JohnDoe/MyProject/main'.
	
		"use separate methods for external projects"
		self 
			fuelMetalevel: spec;
			fileSystemLegacy: spec.
		
		"specify the dependencies between packages and projects"
		spec
			"a package without dependencies:"
			package: 'MyProject-Core';
			package: 'MyProject-Tests' with: [ 
				"Specfiy dependencies using the #requires: directive, you can refer to any name here, in this case to an external project"
				spec requires: #('MyProject-Core' 'FuelMetalevel' 'FileSystemLegacy')].
			
		"using groups certain packages and projects can be loaded conditionally"
		spec 
			"load the tests by default"
			group: 'default' with: #('core' 'test');
			group: 'test'    with: #('MyProject-Tests');
			group: 'core'    with: #('MyProject-Core')]