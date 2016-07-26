versions
version124: spec
	<version: '1.24' imports: #('1.2-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'development'.
		spec description: '1.24 : adding glamour configuration
1.24 : Configuring Versionner with glamour and spec
1.24 : Configuring stable version'.
		spec author: 'JuanPabloSandovalA'.
		spec timestamp: '6/23/2012 01:06'.
		spec 
			package: 'Versionner-Core-Model' with: 'Versionner-Core-Model-JPSA.11';
			package: 'Versionner-Core-Commands' with: 'Versionner-Core-Commands-JuanPabloSandovalAlcocer.12';
			package: 'Versionner-Core-Announcements' with: 'Versionner-Core-Announcements-JuanPabloSandovalAlcocer.4';
			package: 'Versionner-Tests-Core-Model' with: 'Versionner-Tests-Core-Model-JuanPabloSandovalAlcocer.5';
			package: 'Versionner-Tests-Core-Commands' with: 'Versionner-Tests-Core-Commands-JuanPabloSandovalAlcocer.2';
			package: 'Versionner-Help' with: 'Versionner-Help-JuanPabloSandovalAlcocer.3';
			package: 'Versionner-Cache' with: 'Versionner-Cache-JuanPabloSandovalA.12';
			package: 'Versionner-Spec-Browser' with: 'Versionner-Spec-Browser-JuanPabloSandovalA.4';
			package: 'Versionner-GLM-Browser' with: 'Versionner-GLM-Browser-JuanPabloSandovalAlcocer.14';
			package: 'Versionner-Tests-Resources' with: 'Versionner-Tests-Resources-JuanPabloSandovalAlcocer.2'. ].
