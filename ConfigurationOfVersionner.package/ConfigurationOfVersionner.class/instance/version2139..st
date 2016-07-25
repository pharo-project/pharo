versions
version2139: spec
	<version: '2.13.9' imports: #('2.8-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'version 2.13.9'.
		spec author: 'StephaneDucasse'.
		spec timestamp: '3/12/2016 13:14'.
		spec 
			package: 'Versionner-Core-Model' with: 'Versionner-Core-Model-ChristopheDemarey.52';
			package: 'Versionner-Core-Commands' with: 'Versionner-Core-Commands-StephaneDucasse.64';
			package: 'Versionner-Core-Announcements' with: 'Versionner-Core-Announcements-ChristopheDemarey.6';
			package: 'Versionner-Core-DependenciesModel' with: 'Versionner-Core-DependenciesModel-MarcusDenker.104';
			package: 'Versionner-Spec-Browser' with: 'Versionner-Spec-Browser-StephaneDucasse.181';
			package: 'Versionner-Tests-Resources' with: 'Versionner-Tests-Resources-MarcusDenker.18';
			package: 'Versionner-Tests-Core-Model' with: 'Versionner-Tests-Core-Model-ChristopheDemarey.33';
			package: 'Versionner-Tests-Core-Commands' with: 'Versionner-Tests-Core-Commands-MarcusDenker.21';
			package: 'Versionner-Tests-Core-DependenciesModel' with: 'Versionner-Tests-Core-DependenciesModel-MarcusDenker.45';
			package: 'Versionner-Commit' with: 'Versionner-Commit-EstebanLorenzano.31'. ].
