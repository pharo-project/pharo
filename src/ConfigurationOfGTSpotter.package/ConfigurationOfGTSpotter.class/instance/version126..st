versions
version126: spec
	<version: '1.2.6' imports: #('1.2-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'spotter dive into class from senders/implementors result list'.
		spec author: 'EstebanLorenzano'.
		spec timestamp: '6/17/2015 12:04'.
		spec project: 'GlamourCore' with: '3.1.4'.
		spec 
			package: 'GT-Spotter' with: 'GT-Spotter-StephaneDucasse.309';
			package: 'GT-Spotter-EventRecorder' with: 'GT-Spotter-EventRecorder-JurajKubelka.89';
			package: 'GT-SpotterExtensions-Core' with: 'GT-SpotterExtensions-Core-NicolaiHess.147';
			package: 'GT-Tests-Spotter' with: 'GT-Tests-Spotter-TudorGirba.8'. ].
