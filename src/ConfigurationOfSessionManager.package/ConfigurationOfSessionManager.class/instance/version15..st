versions
version15: spec
	<version: '1.5' imports: #('0.1-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'version 1.5
Add a method to register a handler at last position in a category.
Fix SystemSettingsPersistence handler position to be at the last position of the startup list.'.
		spec author: 'ChristopheDemarey'.
		spec timestamp: '3/18/2016 10:19'.
		spec 
			package: #'System-SessionManager' with: 'System-SessionManager-ChristopheDemarey.21';
			package: #'System-SessionManager-Tests' with: 'System-SessionManager-Tests-ChristopheDemarey.9'. ].
