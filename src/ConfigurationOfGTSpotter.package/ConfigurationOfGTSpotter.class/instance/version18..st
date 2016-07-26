versions
version18: spec
	<version: '1.8' imports: #('1.3-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'Fixes in the debugger, inspector and spotter.'.
		spec author: 'AndreiChis'.
		spec timestamp: '1/10/2016 23:53'.
		spec 
			project: 'GlamourCore' with: '3.7';
			project: 'GTEventRecorder' with: '0.7'.
		spec 
			package: 'GT-Spotter' with: 'GT-Spotter-TudorGirba.336';
			package: 'GT-Spotter-EventRecorder' with: 'GT-Spotter-EventRecorder-JurajKubelka.107';
			package: 'GT-SpotterExtensions-Core' with: 'GT-SpotterExtensions-Core-TudorGirba.185';
			package: 'GT-Tests-Spotter' with: 'GT-Tests-Spotter-StefanReichhart.15'. ].
