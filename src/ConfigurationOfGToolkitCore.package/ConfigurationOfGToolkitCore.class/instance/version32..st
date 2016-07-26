versions
version32: spec
	<version: '3.2' imports: #('3.0-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'Update GTools. 
 - bug fixes
 - improvements in the event recorder'.
		spec author: 'AndreiChis'.
		spec timestamp: '11/19/2015 20:58'.
		spec 
			project: 'GTPlaygroundCore' with: '2.3';
			project: 'GTInspectorCore' with: '2.2';
			project: 'GTSpotter' with: '1.4'. ].
