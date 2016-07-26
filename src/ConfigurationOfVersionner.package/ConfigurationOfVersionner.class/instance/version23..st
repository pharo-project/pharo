versions
version23: spec
	<version: '2.3' imports: #('2.3-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'version 2.3'.
		spec author: 'ChristopheDemarey'.
		spec timestamp: '2/14/2014 18:12'.
		spec 
			package: 'Versionner-Core-Model' with: 'Versionner-Core-Model-ChristopheDemarey.36';
			package: 'Versionner-Core-Commands' with: 'Versionner-Core-Commands-ChristopheDemarey.40';
			package: 'Versionner-Core-Announcements' with: 'Versionner-Core-Announcements-ChristopheDemarey.5';
			package: 'Versionner-Core-DependenciesModel' with: 'Versionner-Core-DependenciesModel-ChristopheDemarey.67';
			package: 'Versionner-Spec-Browser' with: 'Versionner-Spec-Browser-ChristopheDemarey.116';
			package: 'Versionner-Tests-Resources' with: 'Versionner-Tests-Resources-ChristopheDemarey.8';
			package: 'Versionner-Tests-Core-Model' with: 'Versionner-Tests-Core-Model-ChristopheDemarey.13';
			package: 'Versionner-Tests-Core-Commands' with: 'Versionner-Tests-Core-Commands-ChristopheDemarey.8';
			package: 'Versionner-Tests-Core-DependenciesModel' with: 'Versionner-Tests-Core-DependenciesModel-ChristopheDemarey.29'. ].
