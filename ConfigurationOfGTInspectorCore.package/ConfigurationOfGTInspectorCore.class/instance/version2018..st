versions
version2018: spec
	<version: '2.0.18' imports: #('2.0-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'version 2.0.18'.
		spec author: 'AndreiChis'.
		spec timestamp: '8/12/2015'.
		spec project: 'GlamourCore' with: '3.1.9'.
		spec 
			package: 'GT-Inspector' with: 'GT-Inspector-MarcusDenker.309';
			package: 'GT-InspectorExtensions-Core' with: 'GT-InspectorExtensions-Core-TorstenBergmann.159';
			package: 'GT-Tests-Inspector' with: 'GT-Tests-Inspector-StefanReichhart.33'. ].
