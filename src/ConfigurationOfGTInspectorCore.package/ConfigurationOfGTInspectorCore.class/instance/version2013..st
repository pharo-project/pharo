versions
version2013: spec
	<version: '2.0.13' imports: #('2.0-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'version 2.0.13'.
		spec author: 'AndreiChis'.
		spec timestamp: '04/29/2015 11:11'.
		spec project: 'GlamourCore' with: '3.1.2'.
		spec 
			package: 'GT-Inspector' with: 'GT-Inspector-SeanDeNigris.306';
			package: 'GT-InspectorExtensions-Core' with: 'GT-InspectorExtensions-Core-TorstenBergmann.145';
			package: 'GT-Tests-Inspector' with: 'GT-Tests-Inspector-StefanReichhart.33'. ].
