versions
version16: spec
	<version: '1.6' imports: #('1.3-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'version 1.6'.
		spec author: 'JurajKubelka'.
		spec timestamp: '1/7/2016 19:28'.
		spec 
			project: 'GlamourCore' with: '3.5';
			project: 'GTEventRecorder' with: '0.5'.
		spec 
			package: 'GT-Spotter' with: 'GT-Spotter-JurajKubelka.334';
			package: 'GT-Spotter-EventRecorder' with: 'GT-Spotter-EventRecorder-JurajKubelka.107';
			package: 'GT-SpotterExtensions-Core' with: 'GT-SpotterExtensions-Core-AndreiChis.172';
			package: 'GT-Tests-Spotter' with: 'GT-Tests-Spotter-StefanReichhart.15'. ].
