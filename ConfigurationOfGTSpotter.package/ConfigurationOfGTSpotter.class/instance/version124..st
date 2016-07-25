versions
version124: spec
	<version: '1.2.4' imports: #('1.2-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'merged with Pharo + fixed RPackage>>spotterActDefault'.
		spec author: 'AndreiChis'.
		spec timestamp: '5/7/2015 15:29'.
		spec project: 'GlamourCore' with: '3.1.3'.
		spec 
			package: 'GT-Spotter' with: 'GT-Spotter-BernardoContreras.308';
			package: 'GT-Spotter-EventRecorder' with: 'GT-Spotter-EventRecorder-JurajKubelka.89';
			package: 'GT-SpotterExtensions-Core' with: 'GT-SpotterExtensions-Core-AndreiChis.146';
			package: 'GT-Tests-Spotter' with: 'GT-Tests-Spotter-TudorGirba.8'. ].
