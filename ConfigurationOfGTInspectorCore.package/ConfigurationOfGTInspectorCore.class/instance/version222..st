versions
version222: spec
	<version: '2.2.2' imports: #('2.0-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'Update GTools: small fixes and improvements (issue 17184)'.
		spec author: 'AndreiChis'.
		spec timestamp: '12/4/2015 11:12'.
		spec project: 'GlamourCore' with: '3.4.2'.
		spec 
			package: 'GT-Inspector' with: 'GT-Inspector-StefanReichhart.355';
			package: 'GT-InspectorExtensions-Core' with: 'GT-InspectorExtensions-Core-AndreiChis.186';
			package: 'GT-Tests-Inspector' with: 'GT-Tests-Inspector-AndreiChis.49'. ].
