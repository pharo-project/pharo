versions
version25: spec
	<version: '2.5' imports: #('1.3-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'GToolkit version 3.16'.
		spec author: 'AndreiChis'.
		spec timestamp: '3/23/2016 14:21'.
		spec 
			project: 'GlamourCore' with: '4.8';
			project: 'GTEventRecorder' with: '0.13'.
		spec 
			package: 'GT-Spotter' with: 'GT-Spotter-YuriyTymchuk.390';
			package: 'GT-Spotter-EventRecorder' with: 'GT-Spotter-EventRecorder-TheIntegrator.109';
			package: 'GT-SpotterExtensions-Core' with: 'GT-SpotterExtensions-Core-AliakseiSyrel.201';
			package: 'GT-Tests-Spotter' with: 'GT-Tests-Spotter-AliakseiSyrel.17'. ].
