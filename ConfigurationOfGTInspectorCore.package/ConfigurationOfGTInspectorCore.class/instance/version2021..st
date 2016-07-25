versions
version2021: spec
	<version: '2.0.21' imports: #('2.0-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'cleanup before remove ThemeIcons'.
		spec author: 'EstebanLorenzano'.
		spec timestamp: '10/2/2015 16:09'.
		spec project: 'GlamourCore' with: '3.2.3'.
		spec 
			package: 'GT-Inspector' with: 'GT-Inspector-AndreiChis.310';
			package: 'GT-InspectorExtensions-Core' with: 'GT-InspectorExtensions-Core-EstebanLorenzano.161';
			package: 'GT-Tests-Inspector' with: 'GT-Tests-Inspector-TudorGirba.34'. ].
