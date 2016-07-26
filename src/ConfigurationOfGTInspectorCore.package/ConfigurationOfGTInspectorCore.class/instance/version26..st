versions
version26: spec
	<version: '2.6' imports: #('2.0-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'Bugfixes in Spotter and Glamour'.
		spec author: 'AndreiChis'.
		spec timestamp: '1/15/2016 16:23'.
		spec project: 'GlamourCore' with: '3.8'.
		spec 
			package: 'GT-Inspector' with: 'GT-Inspector-AndreiChis.368';
			package: 'GT-InspectorExtensions-Core' with: 'GT-InspectorExtensions-Core-YuriyTymchuk.193';
			package: 'GT-Tests-Inspector' with: 'GT-Tests-Inspector-TudorGirba.50'. ].
