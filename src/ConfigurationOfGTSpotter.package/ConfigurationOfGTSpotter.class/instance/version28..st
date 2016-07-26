versions
version28: spec
	<version: '2.8' imports: #('1.3-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'GToolkit version 3.19'.
		spec author: 'AndreiChis'.
		spec timestamp: '4/14/2016 13:57'.
		spec 
			project: 'GlamourCore' with: '4.11';
			project: 'GTEventRecorder' with: '0.14'.
		spec 
			package: 'GT-Spotter' with: 'GT-Spotter-GuillermoPolito.394';
			package: 'GT-Spotter-EventRecorder' with: 'GT-Spotter-EventRecorder-GuillermoPolito.112';
			package: 'GT-SpotterExtensions-Core' with: 'GT-SpotterExtensions-Core-VincentBlondeau.203';
			package: 'GT-Tests-Spotter' with: 'GT-Tests-Spotter-AliakseiSyrel.17'. ].
