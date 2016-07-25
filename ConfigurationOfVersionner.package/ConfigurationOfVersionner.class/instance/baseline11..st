baselines
baseline11: spec
	<version: '1.1-baseline'>

	spec for: #'common' do: [
		spec blessing: #'baseline'.
		spec repository: 'http://ss3.gemstone.com/ss/Versionner'.
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
			package: 'Versionner-Spec-Browser';
			package: 'Versionner-Tests-Resources'. ].
