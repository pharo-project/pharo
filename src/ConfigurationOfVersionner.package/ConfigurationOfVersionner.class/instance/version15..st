versions
version15: spec
	<version: '1.5' imports: #('1.5-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'development'.
		spec description: 'version 1.5'.
		spec author: 'ChristopheDemarey'.
		spec timestamp: '11/27/2013 13:24'.
		spec 
			package: 'Versionner-Core-Model' with: 'Versionner-Core-Model-ChristopheDemarey.31';
			package: 'Versionner-Core-Commands' with: 'Versionner-Core-Commands-ChristopheDemarey.32';
			package: 'Versionner-Core-DependenciesModel' with: 'Versionner-Core-DependenciesModel-ChristopheDemarey.48';
			package: 'Versionner-Core-Announcements' with: 'Versionner-Core-Announcements-ChristopheDemarey.5';
			package: 'Versionner-Spec-Browser' with: 'Versionner-Spec-Browser-ChristopheDemarey.86';
			package: 'Versionner-Tests-Resources' with: 'Versionner-Tests-Resources-ChristopheDemarey.8';
			package: 'Versionner-Tests-Core-Model' with: 'Versionner-Tests-Core-Model-ChristopheDemarey.10';
			package: 'Versionner-Tests-Core-Commands' with: 'Versionner-Tests-Core-Commands-ChristopheDemarey.7';
			package: 'Versionner-Tests-Core-DependenciesModel' with: 'Versionner-Tests-Core-DependenciesModel-ChristopheDemarey.24';
			package: 'Versionner-Help' with: 'Versionner-Help-JuanPabloSandovalAlcocer.3'. ].
