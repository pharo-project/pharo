versions
versionDevelopment: spec
	"version specification for the current development branch, see #version10 for a complete explanation of a version declaration.
	In this case the 'dev' version uses the same baselin as version '1.0':"
	<version: 'dev' imports: #('1.0-baseline') >
	
	"Update this configuration regulrarly with intermediate releases.
	If a version is more stable or should stay accessible copy this 'dev' definition and give it a proper version name on its own.
	For example, in this case you might want to split a new version '1.1' by copying over this definition."
	spec for: #common do: [
		spec 
			description: 'Development Version';
			blessing: #development;
			author: 'John Doe';
			timestamp: '2013-05-09'.
		
		spec
			package: 'MyProject-Core'  with: 'MyProject-Core-JohnDoe.152';
			package: 'MyProject-Tests' with: 'MyProject-Tests-JohnDoe.173';
			"note that for the 'dev' version we rely on the #development version of the external FuleMetalevel project"
			project: 'FuelMetalevel'   with: #development ].