versions
version125: spec
	<version: '1.2.5' imports: #('1.2-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'version 1.2.5'.
		spec author: 'StephaneDucasse'.
		spec timestamp: '6/4/2015 19:49'.
		spec project: 'GlamourCore' with: '3.2-baseline'.
		spec 
			package: 'GT-Spotter' with: 'GT-Spotter-StephaneDucasse.309';
			package: 'GT-Spotter-EventRecorder' with: 'GT-Spotter-EventRecorder-JurajKubelka.89';
			package: 'GT-SpotterExtensions-Core' with: 'GT-SpotterExtensions-Core-AndreiChis.146';
			package: 'GT-Tests-Spotter' with: 'GT-Tests-Spotter-TudorGirba.8'. ].
