versions
version1212: spec
	<version: '1.2.12' imports: #('1.2-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'version 1.2.12'.
		spec author: 'CyrilFerlicot'.
		spec timestamp: '9/11/2015 20:43'.
		spec project: 'GlamourCore' with: '3.1.10'.
		spec 
			package: 'GT-Spotter' with: 'GT-Spotter-CyrilFerlicot.321';
			package: 'GT-Spotter-EventRecorder' with: 'GT-Spotter-EventRecorder-JurajKubelka.96';
			package: 'GT-SpotterExtensions-Core' with: 'GT-SpotterExtensions-Core-MarcusDenker.156';
			package: 'GT-Tests-Spotter' with: 'GT-Tests-Spotter-AndreiChis.9'. ].
