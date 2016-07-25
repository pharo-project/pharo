versions
version1214: spec
	<version: '1.2.14' imports: #('1.2-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'bugfixes'.
		spec author: 'AndreiChis'.
		spec timestamp: '10/8/2015 21:24'.
		spec project: 'GlamourCore' with: '3.2.4'.
		spec 
			package: 'GT-Spotter' with: 'GT-Spotter-CyrilFerlicot.321';
			package: 'GT-Spotter-EventRecorder' with: 'GT-Spotter-EventRecorder-MarcusDenker.97';
			package: 'GT-SpotterExtensions-Core' with: 'GT-SpotterExtensions-Core-EstebanLorenzano.157';
			package: 'GT-Tests-Spotter' with: 'GT-Tests-Spotter-AndreiChis.9'. ].
