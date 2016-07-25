versions
version112: spec
	<version: '1.12' imports: #('1.3-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'GToolkit version 3.10'.
		spec author: 'AndreiChis'.
		spec timestamp: '02/08/2016 15:54'.
		spec 
			project: 'GlamourCore' with: '4.0';
			project: 'GTEventRecorder' with: '0.8'.
		spec 
			package: 'GT-Spotter' with: 'GT-Spotter-AliakseiSyrel.355';
			package: 'GT-Spotter-EventRecorder' with: 'GT-Spotter-EventRecorder-AndreiChis.108';
			package: 'GT-SpotterExtensions-Core' with: 'GT-SpotterExtensions-Core-TudorGirba.189';
			package: 'GT-Tests-Spotter' with: 'GT-Tests-Spotter-StefanReichhart.15'. ].
