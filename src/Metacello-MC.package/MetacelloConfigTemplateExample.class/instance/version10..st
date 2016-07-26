versions
version10: spec
	"The name for this version is solely defined by the following pragma:"
	<version: '1.0' imports: #('1.0-baseline') >
	"Baselines are used to define more complex setups for your project.
	If you want to use external projects and have fine-graind control of the dependencies between packages use the #imports: part.
	See the #baseline10: for more details."
	
	"Using #for:do: with the #common release specifier the following version declaration is valid for all platforms."
	spec for: #common do: [
		spec 
			description: 'Version 1.0 the current stable release';
			blessing: #release;
			author: 'John Doe';
			timestamp: '2013-05-01'.
		
		"Specify the versions for each package and external project defined in the baseline, here the 1.0-baseline defined in the baseline10 method."
		spec
			"For standard Monticello packages simply refere to the full version name without the extension:"
			package: 'MyProject-Core'  with: 'MyProject-Core-JohnDoe.52';
			package: 'MyProject-Tests' with: 'MyProject-Tests-JohnDoe.73';
			
			"External projects versions are specified using #project:with:.
			Note the project name referes to the name used in the Metacello declaration.
			FuelMetalevel is defined in the #fuelMetalevel: method."
			project: 'FuelMetalevel' with: #stable ].
		
	"If you want to specify different version on other platforms add another #for:do: block with a different version identifier."