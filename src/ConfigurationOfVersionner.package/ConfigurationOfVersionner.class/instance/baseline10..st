baselines
baseline10: spec
	<version: '1.0-baseline'>

	spec for: #'common' do: [
		spec blessing: #'baseline'.
		spec repository: 'http://ss3.gemstone.com/ss/Versionner'.
		spec project: 'Glamour' with: [
			spec
					className: 'ConfigurationOfGlamour';
					versionString: #'stable';
					loads: #('Core' 'Morphic');
					repository: 'http://www.squeaksource.com/Glamour'].
		spec
			package: 'Versionner-Core-Model';
			package: 'Versionner-Core-Commands';
			package: 'Versionner-Core-Announcements';
			package: 'Versionner-Tests-Core-Model';
			package: 'Versionner-OB-UI';
			package: 'Versionner-OB-UI-Commands';
			package: 'Versionner-Tests-Core-Commands';
			package: 'Versionner-Help';
			package: 'Versionner-Morphic-UI-Commands';
			package: 'Versionner-Morphic-UI-Widgets';
			package: 'Versionner-Morphic-UI-Browser';
			package: 'Versionner-Morphic-Tests';
			package: 'Versionner-OB-Tests';			
			package: 'Versionner-Cache';
			package: 'Versionner-GLM-Browser';
			package: 'Versionner-Tests-Resources' ].