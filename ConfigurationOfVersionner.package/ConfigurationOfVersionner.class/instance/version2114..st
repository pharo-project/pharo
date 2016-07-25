versions
version2114: spec
	<version: '2.11.4' imports: #('2.8-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'trying to use same remote repository whenever possible'.
		spec author: 'EstebanLorenzano'.
		spec timestamp: '4/28/2015 18:25'.
		spec 
			package: 'Versionner-Core-Model' with: 'Versionner-Core-Model-EstebanLorenzano.50';
			package: 'Versionner-Core-Commands' with: 'Versionner-Core-Commands-EstebanLorenzano.57';
			package: 'Versionner-Core-Announcements' with: 'Versionner-Core-Announcements-MarcusDenker.11';
			package: 'Versionner-Core-DependenciesModel' with: 'Versionner-Core-DependenciesModel-ChristopheDemarey.101';
			package: 'Versionner-Spec-Browser' with: 'Versionner-Spec-Browser-EstebanLorenzano.174';
			package: 'Versionner-Tests-Resources' with: 'Versionner-Tests-Resources-ChristopheDemarey.15';
			package: 'Versionner-Tests-Core-Model' with: 'Versionner-Tests-Core-Model-ChristopheDemarey.33';
			package: 'Versionner-Tests-Core-Commands' with: 'Versionner-Tests-Core-Commands-TheIntegrator.17';
			package: 'Versionner-Tests-Core-DependenciesModel' with: 'Versionner-Tests-Core-DependenciesModel-ChristopheDemarey.42';
			package: 'Versionner-Commit' with: 'Versionner-Commit-EstebanLorenzano.14'. ].
