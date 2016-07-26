versions
version26: spec
	<version: '2.6' imports: #('2.6-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'version 2.6'.
		spec author: 'ChristopheDemarey'.
		spec timestamp: '3/24/2014 10:17'.
		spec 
			package: 'Versionner-Core-Model' with: 'Versionner-Core-Model-ChristopheDemarey.37';
			package: 'Versionner-Core-Commands' with: 'Versionner-Core-Commands-ChristopheDemarey.45';
			package: 'Versionner-Core-Announcements' with: 'Versionner-Core-Announcements-ChristopheDemarey.6';
			package: 'Versionner-Core-DependenciesModel' with: 'Versionner-Core-DependenciesModel-ChristopheDemarey.83';
			package: 'Versionner-Spec-Browser' with: 'Versionner-Spec-Browser-ChristopheDemarey.134';
			package: 'Versionner-Tests-Resources' with: 'Versionner-Tests-Resources-ChristopheDemarey.10';
			package: 'Versionner-Tests-Core-Model' with: 'Versionner-Tests-Core-Model-ChristopheDemarey.23';
			package: 'Versionner-Tests-Core-Commands' with: 'Versionner-Tests-Core-Commands-ChristopheDemarey.10';
			package: 'Versionner-Tests-Core-DependenciesModel' with: 'Versionner-Tests-Core-DependenciesModel-ChristopheDemarey.38'. ].
