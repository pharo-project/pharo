versions
version122: spec
	<version: '1.2.2' imports: #('1.2-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec author: 'AndreiChis'.
		spec timestamp: '04/29/2015 11:09'.
		spec project: 'GlamourCore' with: '3.1.2'.
		spec 
			package: 'GT-Spotter' with: 'GT-Spotter-StefanReichhart.307';
			package: 'GT-Spotter-EventRecorder' with: 'GT-Spotter-EventRecorder-JurajKubelka.89';
			package: 'GT-SpotterExtensions-Core' with: 'GT-SpotterExtensions-Core-TudorGirba.142';
			package: 'GT-Tests-Spotter' with: 'GT-Tests-Spotter-TudorGirba.8' ].