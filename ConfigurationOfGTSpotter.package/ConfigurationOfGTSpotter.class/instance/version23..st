versions
version23: spec
	<version: '2.3' imports: #('1.3-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'GToolkit version 3.14'.
		spec author: 'AndreiChis'.
		spec timestamp: '2/28/2016 12:11'.
		spec 
			project: 'GlamourCore' with: '4.4';
			project: 'GTEventRecorder' with: '0.10'.
		spec 
			package: 'GT-Spotter' with: 'GT-Spotter-TudorGirba.379';
			package: 'GT-Spotter-EventRecorder' with: 'GT-Spotter-EventRecorder-TheIntegrator.109';
			package: 'GT-SpotterExtensions-Core' with: 'GT-SpotterExtensions-Core-TudorGirba.197';
			package: 'GT-Tests-Spotter' with: 'GT-Tests-Spotter-StefanReichhart.15'. ].
