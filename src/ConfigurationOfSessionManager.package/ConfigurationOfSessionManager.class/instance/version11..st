versions
version11: spec
	<version: '1.1' imports: #('0.1-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'version 1.1 handling double registration and class rename'.
		spec author: 'ChristopheDemarey'.
		spec timestamp: '2/3/2016 08:57'.
		spec 
			package: #'System-SessionManager' with: 'System-SessionManager-ChristopheDemarey.15';
			package: #'System-SessionManager-Tests' with: 'System-SessionManager-Tests-ChristopheDemarey.8'. ].
