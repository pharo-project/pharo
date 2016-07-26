baselines
baseline12: spec
	<version: '1.2-baseline'>

	spec for: #common do: [
		spec blessing: #baseline.
		spec repository: 'http://ss3.gemstone.com/ss/Versionner'.	
		spec project: 'Metacello ToolBox' with: [
				spec
					className: 'ConfigurationOfMetacello';
					version: #'stable';
					loads: #('Metacello-ToolBox');
					repository: 'http://seaside.gemstone.com/ss/metacello';
					repository: 'http://www.squeaksource.com/metacello'.].
		spec
			project: 'Glamour for Versionner' with: [
				spec
					className: 'ConfigurationOfGlamour';
					file: 'ConfigurationOfGlamour';
					versionString: #'stable';
					loads: #('Core' 'Morphic');
					repository: 'http://www.squeaksource.com/Glamour'].
		spec
			project: 'Spec for Versionner' with: [
				spec
					className: 'ConfigurationOfSpec';
					file: 'ConfigurationOfSpec';
					version:#stable;
					repository: 'http://ss3.gemstone.com/ss/Spec' ].
		spec 
			package: 'Versionner-Core-Model' with: [ spec requires: 'Metacello ToolBox' ];
			package: 'Versionner-Core-Commands' with:[ spec requires: #('Versionner-Core-Model')];
			package: 'Versionner-Core-Announcements' with:[ spec requires: #('Versionner-Core-Model')];
			package: 'Versionner-Tests-Core-Model' with:[ spec requires: #('Versionner-Core-Model')];
			package: 'Versionner-Tests-Core-Commands' with:[ spec requires: #('Versionner-Core-Model' 'Versionner-Core-Commands')];
			package: 'Versionner-Help';	
			package: 'Versionner-Cache' with:[ spec requires: #('Versionner-Core-Model' 'Versionner-Core-Commands')];
			package: 'Versionner-Spec-Browser' with:[ spec requires: 'Spec for Versionner'];
			package: 'Versionner-GLM-Browser' with:[ spec requires: 'Glamour for Versionner'];
			package: 'Versionner-Tests-Resources'.

		spec group: 'Core' with: #('Versionner-Core-Model' 'Versionner-Core-Commands' 'Versionner-Core-Announcements' 'Versionner-Cache').
		spec group: 'GLM' with: #('Versionner-GLM-Browser').
		spec group: 'Spec' with: #('Versionner-Spec-Browser').
		
	spec].