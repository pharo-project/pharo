versions
version110: spec
	<version: '1.10' imports: #('1.3-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'Bugfixes in inspector and debugger'.
		spec author: 'AndreiChis'.
		spec timestamp: '1/15/2016 18:14'.
		spec 
			project: 'GlamourCore' with: '3.9';
			project: 'GTEventRecorder' with: '0.7'.
		spec 
			package: 'GT-Spotter' with: 'GT-Spotter-TudorGirba.336';
			package: 'GT-Spotter-EventRecorder' with: 'GT-Spotter-EventRecorder-JurajKubelka.107';
			package: 'GT-SpotterExtensions-Core' with: 'GT-SpotterExtensions-Core-TudorGirba.187';
			package: 'GT-Tests-Spotter' with: 'GT-Tests-Spotter-StefanReichhart.15'. ].
