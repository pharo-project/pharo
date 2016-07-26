versions
version13: spec
	<version: '1.3' imports: #('0.1-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'version 1.3
- SessionManager lacks the ability check if a class is registered or not
https://pharo.fogbugz.com/f/cases/17663
- Add methods to easily register user categories'.
		spec author: 'ChristopheDemarey'.
		spec timestamp: '2/23/2016 11:50'.
		spec 
			package: #'System-SessionManager' with: 'System-SessionManager-ChristopheDemarey.18';
			package: #'System-SessionManager-Tests' with: 'System-SessionManager-Tests-ChristopheDemarey.9'. ].
