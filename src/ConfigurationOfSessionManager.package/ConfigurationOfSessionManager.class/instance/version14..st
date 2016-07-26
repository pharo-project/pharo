versions
version14: spec
	<version: '1.4' imports: #('0.1-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'version 1.4
		fix startup order for FileLocator and WorldMorph'.
		spec author: 'ChristopheDemarey'.
		spec timestamp: '3/17/2016 15:49'.
		spec 
			package: #'System-SessionManager' with: 'System-SessionManager-ChristopheDemarey.20';
			package: #'System-SessionManager-Tests' with: 'System-SessionManager-Tests-ChristopheDemarey.9'. ].
