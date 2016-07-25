versions
version26: spec
	<version: '2.6' imports: #('1.3-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'GToolkit version 3.17'.
		spec author: 'AndreiChis'.
		spec timestamp: '3/29/2016 23:44'.
		spec 
			project: 'GlamourCore' with: '4.9';
			project: 'GTEventRecorder' with: '0.14'.
		spec 
			package: 'GT-Spotter' with: 'GT-Spotter-AndreiChis.391';
			package: 'GT-Spotter-EventRecorder' with: 'GT-Spotter-EventRecorder-TheIntegrator.109';
			package: 'GT-SpotterExtensions-Core' with: 'GT-SpotterExtensions-Core-AliakseiSyrel.201';
			package: 'GT-Tests-Spotter' with: 'GT-Tests-Spotter-AliakseiSyrel.17'. ].
