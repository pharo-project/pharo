versions
version210: spec
	<version: '2.10' imports: #('1.3-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'GToolkit version 3.21 (case 18112)'.
		spec author: 'AndreiChis'.
		spec timestamp: '4/27/2016 19:10'.
		spec 
			project: 'GlamourCore' with: '4.13';
			project: 'GTEventRecorder' with: '0.15'.
		spec 
			package: 'GT-Spotter' with: 'GT-Spotter-TudorGirba.397';
			package: 'GT-Spotter-EventRecorder' with: 'GT-Spotter-EventRecorder-GuillermoPolito.112';
			package: 'GT-SpotterExtensions-Core' with: 'GT-SpotterExtensions-Core-VincentBlondeau.203';
			package: 'GT-Tests-Spotter' with: 'GT-Tests-Spotter-AliakseiSyrel.17'. ].
