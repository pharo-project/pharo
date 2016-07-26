versions
version2022: spec
	<version: '2.0.22' imports: #('2.0-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'bugfixes and examples'.
		spec author: 'AndreiChis'.
		spec timestamp: '10/15/2015 16:26'.
		spec project: 'GlamourCore' with: '3.2.5'.
		spec 
			package: 'GT-Inspector' with: 'GT-Inspector-AndreiChis.344';
			package: 'GT-InspectorExtensions-Core' with: 'GT-InspectorExtensions-Core-AndreiChis.178';
			package: 'GT-Tests-Inspector' with: 'GT-Tests-Inspector-StefanReichhart.41'. ].
