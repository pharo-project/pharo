versions
version211: spec
	<version: '2.11' imports: #('1.3-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'GToolkit version 3.22 (case 17948)'.
		spec author: 'AndreiChis'.
		spec timestamp: '4/28/2016 16:25'.
		spec 
			project: 'GlamourCore' with: '4.15';
			project: 'GTEventRecorder' with: '0.17'.
		spec 
			package: 'GT-Spotter' with: 'GT-Spotter-TudorGirba.397';
			package: 'GT-Spotter-EventRecorder' with: 'GT-Spotter-EventRecorder-GuillermoPolito.112';
			package: 'GT-SpotterExtensions-Core' with: 'GT-SpotterExtensions-Core-VincentBlondeau.203';
			package: 'GT-Tests-Spotter' with: 'GT-Tests-Spotter-AliakseiSyrel.17'. ].
