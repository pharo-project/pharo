versions
version212: spec
	<version: '2.12' imports: #('1.3-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'GToolkit Version 3.23'.
		spec author: 'AndreiChis'.
		spec timestamp: '6/27/2016 12:09'.
		spec 
			project: 'GlamourCore' with: '4.17';
			project: 'GTEventRecorder' with: '0.18'.
		spec 
			package: 'GT-Spotter' with: 'GT-Spotter-TudorGirba.400';
			package: 'GT-Spotter-EventRecorder' with: 'GT-Spotter-EventRecorder-GuillermoPolito.112';
			package: 'GT-SpotterExtensions-Core' with: 'GT-SpotterExtensions-Core-TudorGirba.209';
			package: 'GT-Tests-Spotter' with: 'GT-Tests-Spotter-AndreiChis.19'. ].
