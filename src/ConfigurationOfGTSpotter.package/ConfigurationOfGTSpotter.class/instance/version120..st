versions
version120: spec
	<version: '1.2.0' imports: #('1.2-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec author: 'AndreiChis'.
		spec timestamp: '04/08/2015 21:57'.
		spec project: 'GlamourCore' with: '3.1.0'.
		spec 
			package: 'GT-Spotter' with: 'GT-Spotter-TudorGirba.305';
			package: 'GT-Spotter-EventRecorder' with: 'GT-Spotter-EventRecorder-JurajKubelka.85';
			package: 'GT-SpotterExtensions-Core' with: 'GT-SpotterExtensions-Core-AndreiChis.138';
			package: 'GT-Tests-Spotter' with: 'GT-Tests-Spotter-StefanReichhart.7' ].