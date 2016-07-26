versions
version1213: spec
	<version: '1.2.13' imports: #('1.2-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'cleanup before remove ThemeIcons'.
		spec author: 'EstebanLorenzano'.
		spec timestamp: '10/2/2015 16:11'.
		spec project: 'GlamourCore' with: '3.2.3'.
		spec 
			package: 'GT-Spotter' with: 'GT-Spotter-CyrilFerlicot.321';
			package: 'GT-Spotter-EventRecorder' with: 'GT-Spotter-EventRecorder-JurajKubelka.96';
			package: 'GT-SpotterExtensions-Core' with: 'GT-SpotterExtensions-Core-EstebanLorenzano.157';
			package: 'GT-Tests-Spotter' with: 'GT-Tests-Spotter-AndreiChis.9'. ].
