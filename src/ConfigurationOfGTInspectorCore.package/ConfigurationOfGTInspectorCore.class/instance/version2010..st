versions
version2010: spec
	<version: '2.0.10' imports: #('2.0-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'version 2.0.10'.
		spec author: 'AndreiChis'.
		spec timestamp: '01/16/2015 19:10'.
		spec project: 'GlamourCore' with: '3.0.10'.
		spec 
			package: 'GT-Inspector' with: 'GT-Inspector-TudorGirba.284';
			package: 'GT-InspectorExtensions-Core' with: 'GT-InspectorExtensions-Core-TudorGirba.93';
			package: 'GT-Tests-Inspector' with: 'GT-Tests-Inspector-AndreiChis.24'. ].
