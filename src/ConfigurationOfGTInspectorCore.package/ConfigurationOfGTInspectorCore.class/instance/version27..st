versions
version27: spec
	<version: '2.7' imports: #('2.0-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'Bugfixes in inspector and debugger'.
		spec author: 'AndreiChis'.
		spec timestamp: '1/15/2016 18:14'.
		spec project: 'GlamourCore' with: '3.9'.
		spec 
			package: 'GT-Inspector' with: 'GT-Inspector-AndreiChis.370';
			package: 'GT-InspectorExtensions-Core' with: 'GT-InspectorExtensions-Core-YuriyTymchuk.193';
			package: 'GT-Tests-Inspector' with: 'GT-Tests-Inspector-TudorGirba.50'. ].
