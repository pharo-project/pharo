versions
version29: spec
	<version: '2.9' imports: #('2.7-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'version 2.9'.
		spec author: 'ChristopheDemarey'.
		spec timestamp: '3/20/2015 11:45'.
		spec 
			package: 'Versionner-Core-Model' with: 'Versionner-Core-Model-ChristopheDemarey.49';
			package: 'Versionner-Core-Commands' with: 'Versionner-Core-Commands-ChristopheDemarey.53';
			package: 'Versionner-Core-Announcements' with: 'Versionner-Core-Announcements-MarcusDenker.11';
			package: 'Versionner-Core-DependenciesModel' with: 'Versionner-Core-DependenciesModel-TheIntegrator.95';
			package: 'Versionner-Spec-Browser' with: 'Versionner-Spec-Browser-ChristopheDemarey.173';
			package: 'Versionner-Tests-Resources' with: 'Versionner-Tests-Resources-MarcusDenker.14';
			package: 'Versionner-Tests-Core-Model' with: 'Versionner-Tests-Core-Model-ChristopheDemarey.33';
			package: 'Versionner-Tests-Core-Commands' with: 'Versionner-Tests-Core-Commands-TheIntegrator.17';
			package: 'Versionner-Tests-Core-DependenciesModel' with: 'Versionner-Tests-Core-DependenciesModel-MarcusDenker.40'. ].
