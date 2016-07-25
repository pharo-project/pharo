versions
version2017: spec
	<version: '2.0.17' imports: #('2.0-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'version 2.0.17'.
		spec author: 'AndreiChis'.
		spec timestamp: '7/3/2015'.
		spec project: 'GlamourCore' with: '3.1.8'.
		spec 
			package: 'GT-Inspector' with: 'GT-Inspector-AndreiChis.308';
			package: 'GT-InspectorExtensions-Core' with: 'GT-InspectorExtensions-Core-VincentBlondeau.150';
			package: 'GT-Tests-Inspector' with: 'GT-Tests-Inspector-StefanReichhart.33'. ].
