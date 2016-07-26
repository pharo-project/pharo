versions
version2016: spec
	<version: '2.0.16' imports: #('2.0-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'version 2.0.16'.
		spec author: 'AndreiChis'.
		spec timestamp: '7/1/2015'.
		spec project: 'GlamourCore' with: '3.1.7'.
		spec 
			package: 'GT-Inspector' with: 'GT-Inspector-AndreiChis.308';
			package: 'GT-InspectorExtensions-Core' with: 'GT-InspectorExtensions-Core-AndreiChis.149';
			package: 'GT-Tests-Inspector' with: 'GT-Tests-Inspector-StefanReichhart.33'. ].
