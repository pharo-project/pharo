versions
version2111: spec
	<version: '2.11.1' imports: #('2.8-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'releases also includes comments (this comment)'.
		spec author: 'EstebanLorenzano'.
		spec timestamp: '4/27/2015 20:34'.
		spec 
			package: 'Versionner-Core-Model' with: 'Versionner-Core-Model-ChristopheDemarey.49';
			package: 'Versionner-Core-Commands' with: 'Versionner-Core-Commands-ChristopheDemarey.53';
			package: 'Versionner-Core-Announcements' with: 'Versionner-Core-Announcements-MarcusDenker.11';
			package: 'Versionner-Core-DependenciesModel' with: 'Versionner-Core-DependenciesModel-TheIntegrator.95';
			package: 'Versionner-Spec-Browser' with: 'Versionner-Spec-Browser-ChristopheDemarey.173';
			package: 'Versionner-Tests-Resources' with: 'Versionner-Tests-Resources-MarcusDenker.14';
			package: 'Versionner-Tests-Core-Model' with: 'Versionner-Tests-Core-Model-ChristopheDemarey.33';
			package: 'Versionner-Tests-Core-Commands' with: 'Versionner-Tests-Core-Commands-TheIntegrator.17';
			package: 'Versionner-Tests-Core-DependenciesModel' with: 'Versionner-Tests-Core-DependenciesModel-MarcusDenker.40';
			package: 'Versionner-Commit' with: 'Versionner-Commit-EstebanLorenzano.4'. ].
