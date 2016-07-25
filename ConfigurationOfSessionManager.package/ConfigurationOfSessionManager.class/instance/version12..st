versions
version12: spec
	<version: '1.2' imports: #('0.1-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'version 1.2
add convenient method to register in user category'.
		spec author: 'ChristopheDemarey'.
		spec timestamp: '2/11/2016 10:18'.
		spec 
			package: #'System-SessionManager' with: 'System-SessionManager-ChristopheDemarey.17';
			package: #'System-SessionManager-Tests' with: 'System-SessionManager-Tests-ChristopheDemarey.8'. ].
