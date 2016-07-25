versions
version135: spec
	<version: '1.35' imports: #('1.3-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'development'.
		spec description: '1.35 : Remove glamour support. Port to Pharo 2.0.
1.35 :
Fixing issues in Pharo 2.0; current test estatus:

62 run, 45 passes, 0 expected failures, 14 failures, 3 errors, 0 unexpected passes
1.35 :
Fixing issues in Pharo 2.0; current test estatus:

62 run, 45 passes, 0 expected failures, 14 failures, 3 errors, 0 unexpected passes'.
		spec author: 'JuanPabloSandovalA'.
		spec timestamp: '8/15/2012 15:36'.
		spec 
			project: 'Metacello ToolBox' with: #'stable';
			project: 'Spec for Versionner' with: #'bleedingEdge'.
		spec 
			package: 'Versionner-Core-Model' with: 'Versionner-Core-Model-JuanPabloSandovalA.17';
			package: 'Versionner-Core-Commands' with: 'Versionner-Core-Commands-JuanPabloSandovalAlcocer.16';
			package: 'Versionner-Core-Announcements' with: 'Versionner-Core-Announcements-JuanPabloSandovalAlcocer.4';
			package: 'Versionner-Tests-Core-Model' with: 'Versionner-Tests-Core-Model-JuanPabloSandovalA.6';
			package: 'Versionner-Tests-Core-Commands' with: 'Versionner-Tests-Core-Commands-JuanPabloSandovalA.3';
			package: 'Versionner-Help' with: 'Versionner-Help-JuanPabloSandovalAlcocer.3';
			package: 'Versionner-Cache' with: 'Versionner-Cache-JuanPabloSandovalAlcocer.13';
			package: 'Versionner-Spec-Browser' with: 'Versionner-Spec-Browser-JuanPabloSandovalAlcocer.11';
			package: 'Versionner-Tests-Resources' with: 'Versionner-Tests-Resources-JuanPabloSandovalAlcocer.2'. ].
