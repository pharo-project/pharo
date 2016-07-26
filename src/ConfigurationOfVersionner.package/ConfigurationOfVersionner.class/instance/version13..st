versions
version13: spec
	<version: '1.3' imports: #('1.0-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'development'.
		spec description: '1.2 : Adding a new icon for Load
1.3 : Changing trigger in the world menu'.
		spec author: 'JPSA'.
		spec timestamp: '4/20/2012 11:13'.
		spec 
			package: 'Versionner-Core-Model' with: 'Versionner-Core-Model-JuanPabloSandovalAlcocer.10';
			package: 'Versionner-Core-Commands' with: 'Versionner-Core-Commands-JuanPabloSandovalAlcocer.12';
			package: 'Versionner-Core-Announcements' with: 'Versionner-Core-Announcements-JuanPabloSandovalAlcocer.4';
			package: 'Versionner-Tests-Core-Model' with: 'Versionner-Tests-Core-Model-JuanPabloSandovalAlcocer.5';
			package: 'Versionner-OB-UI' with: 'Versionner-OB-UI-JuanPabloSandovalAlcocer.10';
			package: 'Versionner-OB-UI-Commands' with: 'Versionner-OB-UI-Commands-JuanPabloSandovalAlcocer.3';
			package: 'Versionner-Tests-Core-Commands' with: 'Versionner-Tests-Core-Commands-JuanPabloSandovalAlcocer.2';
			package: 'Versionner-Help' with: 'Versionner-Help-JuanPabloSandovalAlcocer.3';
			package: 'Versionner-Morphic-UI-Commands' with: 'Versionner-Morphic-UI-Commands-JuanPabloSandovalAlcocer.4';
			package: 'Versionner-Morphic-UI-Widgets' with: 'Versionner-Morphic-UI-Widgets-JuanPabloSandovalAlcocer.3';
			package: 'Versionner-Morphic-UI-Browser' with: 'Versionner-Morphic-UI-Browser-JPSA.10';
			package: 'Versionner-Morphic-Tests' with: 'Versionner-Morphic-Tests-JuanPabloSandovalAlcocer.4';
			package: 'Versionner-OB-Tests' with: 'Versionner-OB-Tests-JuanPabloSandovalAlcocer.5';
			package: 'Versionner-Cache' with: 'Versionner-Cache-JuanPabloSandovalAlcocer.7';
			package: 'Versionner-GLM-Browser' with: 'Versionner-GLM-Browser-JPSA.8';
			package: 'Versionner-Tests-Resources' with: 'Versionner-Tests-Resources-JuanPabloSandovalAlcocer.2'. ].
