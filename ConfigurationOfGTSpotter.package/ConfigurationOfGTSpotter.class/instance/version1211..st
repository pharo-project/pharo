versions
version1211: spec
	<version: '1.2.11' imports: #('1.2-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'version 1.2.11'.
		spec author: 'AndreiChis'.
		spec timestamp: '9/2/2015 14:36'.
		spec project: 'GlamourCore' with: '3.1.10'.
		spec 
			package: 'GT-Spotter' with: 'GT-Spotter-AndreiChis.318';
			package: 'GT-Spotter-EventRecorder' with: 'GT-Spotter-EventRecorder-JurajKubelka.96';
			package: 'GT-SpotterExtensions-Core' with: 'GT-SpotterExtensions-Core-MarcusDenker.156';
			package: 'GT-Tests-Spotter' with: 'GT-Tests-Spotter-AndreiChis.9'. ].
