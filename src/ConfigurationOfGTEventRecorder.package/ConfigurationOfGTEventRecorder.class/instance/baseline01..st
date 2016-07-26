baselines
baseline01: spec
	<version: '0.1-baseline'>

	spec for: #'common' do: [
		spec blessing: #'baseline'.
		spec repository: 'http://smalltalkhub.com/mc/Moose/GToolkit/main/'.
		spec project: 'Ston' with: [
				spec
					className: #ConfigurationOfSton;
					versionString: #'stable';
					repository: 'http://ss3.gemstone.com/ss/STON' ].
		spec 
			package: #'GT-EventRecorder' with: [
				spec requires: #('Ston' ). ];
			package: 'GT-EventRecorder-Tests' with: [
				spec requires: #(#'GT-EventRecorder' ). ].
		spec 
			group: 'Core' with: #('GT-EventRecorder');
			group: 'Tests' with: #('GT-EventRecorder-Tests');
			group: 'default' with: #('Core' 'Tests') ].
