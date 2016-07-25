versions
version134: spec
	<version: '1.34' imports: #('1.2-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'development'.
		spec description: '1.33 :

Fixing issue 1 committing a config commits twice.

Now when a configuration an initial version (1.0) is created. And the configuration is saved in the repository automatically.'.
		spec author: 'JuanPabloSandovalAlcocer'.
		spec timestamp: '8/2/2012 15:36'.
		spec 
			project: 'Metacello ToolBox' with: #'stable';
			project: 'Glamour for Versionner' with: #'stable';
			project: 'Spec for Versionner' with: #'stable'.
		spec 
			package: 'Versionner-Core-Model' with: 'Versionner-Core-Model-JuanPabloSandovalAlcocer.13';
			package: 'Versionner-Core-Commands' with: 'Versionner-Core-Commands-JuanPabloSandovalAlcocer.16';
			package: 'Versionner-Core-Announcements' with: 'Versionner-Core-Announcements-JuanPabloSandovalAlcocer.4';
			package: 'Versionner-Tests-Core-Model' with: 'Versionner-Tests-Core-Model-JuanPabloSandovalAlcocer.5';
			package: 'Versionner-Tests-Core-Commands' with: 'Versionner-Tests-Core-Commands-JuanPabloSandovalAlcocer.2';
			package: 'Versionner-Help' with: 'Versionner-Help-JuanPabloSandovalAlcocer.3';
			package: 'Versionner-Cache' with: 'Versionner-Cache-JuanPabloSandovalAlcocer.13';
			package: 'Versionner-Spec-Browser' with: 'Versionner-Spec-Browser-JuanPabloSandovalAlcocer.11';
			package: 'Versionner-GLM-Browser' with: 'Versionner-GLM-Browser-JuanPabloSandovalAlcocer.18';
			package: 'Versionner-Tests-Resources' with: 'Versionner-Tests-Resources-JuanPabloSandovalAlcocer.2'. ].
