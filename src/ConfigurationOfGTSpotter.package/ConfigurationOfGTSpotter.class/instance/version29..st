versions
version29: spec
	<version: '2.9' imports: #('1.3-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'GToolkit version 3.20 (case 18022)'.
		spec author: 'AndreiChis'.
		spec timestamp: '4/18/2016 22:57'.
		spec 
			project: 'GlamourCore' with: '4.12';
			project: 'GTEventRecorder' with: '0.15'.
		spec 
			package: 'GT-Spotter' with: 'GT-Spotter-GuillermoPolito.394';
			package: 'GT-Spotter-EventRecorder' with: 'GT-Spotter-EventRecorder-GuillermoPolito.112';
			package: 'GT-SpotterExtensions-Core' with: 'GT-SpotterExtensions-Core-VincentBlondeau.203';
			package: 'GT-Tests-Spotter' with: 'GT-Tests-Spotter-AliakseiSyrel.17'. ].
