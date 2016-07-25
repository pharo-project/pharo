versions
version22: spec
	<version: '2.2' imports: #('1.3-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'GTools version 3.13'.
		spec author: 'AndreiChis'.
		spec timestamp: '2/23/2016 17:57'.
		spec 
			project: 'GlamourCore' with: '4.3';
			project: 'GTEventRecorder' with: '0.9'.
		spec 
			package: 'GT-Spotter' with: 'GT-Spotter-TudorGirba.374';
			package: 'GT-Spotter-EventRecorder' with: 'GT-Spotter-EventRecorder-TheIntegrator.109';
			package: 'GT-SpotterExtensions-Core' with: 'GT-SpotterExtensions-Core-TudorGirba.193';
			package: 'GT-Tests-Spotter' with: 'GT-Tests-Spotter-StefanReichhart.15'. ].
