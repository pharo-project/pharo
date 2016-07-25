versions
version10: spec
	<version: '1.0' imports: #('0.1-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'version 1.0'.
		spec author: 'ChristopheDemarey'.
		spec timestamp: '1/15/2016 16:17'.
		spec 
			package: #'System-SessionManager' with: 'System-SessionManager-ChristopheDemarey.12';
			package: #'System-SessionManager-Tests' with: 'System-SessionManager-Tests-ChristopheDemarey.6'. ].
