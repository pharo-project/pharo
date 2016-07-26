versions
version2012: spec
	<version: '2.0.12' imports: #('2.0-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'version 2.0.12'.
		spec author: 'AndreiChis'.
		spec timestamp: '04/11/2015 18:02'.
		spec project: 'GlamourCore' with: '3.1.1'.
		spec 
			package: 'GT-Inspector' with: 'GT-Inspector-TudorGirba.305';
			package: 'GT-InspectorExtensions-Core' with: 'GT-InspectorExtensions-Core-TudorGirba.144';
			package: 'GT-Tests-Inspector' with: 'GT-Tests-Inspector-StefanReichhart.33'. ].
