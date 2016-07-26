versions
version27: spec
	<version: '2.7' imports: #('2.7-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'version 2.7'.
		spec author: 'ChristopheDemarey'.
		spec timestamp: '3/19/2015 15:32'.
		spec 
			package: 'Versionner-Core-Model' with: 'Versionner-Core-Model-ChristopheDemarey.49';
			package: 'Versionner-Core-Commands' with: 'Versionner-Core-Commands-MarcusDenker.49';
			package: 'Versionner-Core-Announcements' with: 'Versionner-Core-Announcements-MarcusDenker.11';
			package: 'Versionner-Core-DependenciesModel' with: 'Versionner-Core-DependenciesModel-TheIntegrator.95';
			package: 'Versionner-Spec-Browser' with: 'Versionner-Spec-Browser-ChristopheDemarey.169';
			package: 'Versionner-Tests-Resources' with: 'Versionner-Tests-Resources-MarcusDenker.14';
			package: 'Versionner-Tests-Core-Model' with: 'Versionner-Tests-Core-Model-ChristopheDemarey.31';
			package: 'Versionner-Tests-Core-Commands' with: 'Versionner-Tests-Core-Commands-TheIntegrator.17';
			package: 'Versionner-Tests-Core-DependenciesModel' with: 'Versionner-Tests-Core-DependenciesModel-MarcusDenker.40'. ].
