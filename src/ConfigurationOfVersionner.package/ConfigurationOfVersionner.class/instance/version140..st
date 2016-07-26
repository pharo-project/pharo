versions
version140: spec
	<version: '1.40' imports: #('1.5-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'development'.
		spec description: ''.
		spec author: 'ChristopheDemarey'.
		spec timestamp: '03/25/2013 16:15'.
		spec 
			package: 'Versionner-Core-Model' with: 'Versionner-Core-Model-ChristopheDemarey.23';
			package: 'Versionner-Core-DependenciesModel' with: 'Versionner-Core-DependenciesModel-ChristopheDemarey.7';
			package: 'Versionner-Core-Commands' with: 'Versionner-Core-Commands-ChristopheDemarey.19';
			package: 'Versionner-Core-Announcements' with: 'Versionner-Core-Announcements-JuanPabloSandovalAlcocer.4';
			package: 'Versionner-Spec-Browser' with: 'Versionner-Spec-Browser-ChristopheDemarey.24';
			package: 'Versionner-Tests-Resources' with: 'Versionner-Tests-Resources-ChristopheDemarey.4';
			package: 'Versionner-Tests-Core-Model' with: 'Versionner-Tests-Core-Model-ChristopheDemarey.7';
			package: 'Versionner-Tests-Core-Commands' with: 'Versionner-Tests-Core-Commands-JuanPabloSandovalAlcocer.2';
			package: 'Versionner-Tests-Core-DependenciesModel' with: 'Versionner-Tests-Core-DependenciesModel-ChristopheDemarey.1';
			package: 'Versionner-Help' with: 'Versionner-Help-JuanPabloSandovalAlcocer.3'.  ].
