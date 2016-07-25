versions
version27: spec
	<version: '2.7' imports: #('1.3-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'GToolkit version 3.18'.
		spec author: 'AndreiChis'.
		spec timestamp: '4/8/2016 10:36'.
		spec 
			project: 'GlamourCore' with: '4.10';
			project: 'GTEventRecorder' with: '0.14'.
		spec 
			package: 'GT-Spotter' with: 'GT-Spotter-AndreiChis.392';
			package: 'GT-Spotter-EventRecorder' with: 'GT-Spotter-EventRecorder-TheIntegrator.109';
			package: 'GT-SpotterExtensions-Core' with: 'GT-SpotterExtensions-Core-AndreiChis.202';
			package: 'GT-Tests-Spotter' with: 'GT-Tests-Spotter-AliakseiSyrel.17'. ].
