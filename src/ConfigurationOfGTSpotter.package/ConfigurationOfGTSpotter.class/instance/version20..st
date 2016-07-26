versions
version20: spec
	<version: '2.0' imports: #('1.3-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'GTools version 3.11'.
		spec author: 'AndreiChis'.
		spec timestamp: '2/18/2016 14:21'.
		spec 
			project: 'GlamourCore' with: '4.1';
			project: 'GTEventRecorder' with: '0.9'.
		spec 
			package: 'GT-Spotter' with: 'GT-Spotter-TudorGirba.359';
			package: 'GT-Spotter-EventRecorder' with: 'GT-Spotter-EventRecorder-AndreiChis.108';
			package: 'GT-SpotterExtensions-Core' with: 'GT-SpotterExtensions-Core-StefanReichhart.190';
			package: 'GT-Tests-Spotter' with: 'GT-Tests-Spotter-StefanReichhart.15'. ].
