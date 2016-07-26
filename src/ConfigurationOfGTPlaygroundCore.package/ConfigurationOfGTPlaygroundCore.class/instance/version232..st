versions
version232: spec
	<version: '2.3.2' imports: #('2.1-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'Update GTools: small fixes and improvements (issue 17184)'.
		spec author: 'AndreiChis'.
		spec timestamp: '12/4/2015 11:12'.
		spec project: 'GTInspectorCore' with: '2.2.2'.
		spec 
			package: 'GT-Playground' with: 'GT-Playground-AndreiChis.117';
			package: 'GT-Tests-Playground' with: 'GT-Tests-Playground-StefanReichhart.4'. ].
