versions
version21: spec
	<version: '2.1' imports: #('1.3-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'GToolkit version 3.12'.
		spec author: 'AndreiChis'.
		spec timestamp: '2/20/2016 00:11'.
		spec 
			project: 'GlamourCore' with: '4.2';
			project: 'GTEventRecorder' with: '0.9'.
		spec 
			package: 'GT-Spotter' with: 'GT-Spotter-AliakseiSyrel.365';
			package: 'GT-Spotter-EventRecorder' with: 'GT-Spotter-EventRecorder-AndreiChis.108';
			package: 'GT-SpotterExtensions-Core' with: 'GT-SpotterExtensions-Core-TudorGirba.193';
			package: 'GT-Tests-Spotter' with: 'GT-Tests-Spotter-StefanReichhart.15'. ].
