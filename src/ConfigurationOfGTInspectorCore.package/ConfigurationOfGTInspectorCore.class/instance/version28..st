versions
version28: spec
	<version: '2.8' imports: #('2.0-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'Bugfixes and improvements.'.
		spec author: 'AndreiChis'.
		spec timestamp: '1/18/2016 13:24'.
		spec project: 'GlamourCore' with: '3.10'.
		spec 
			package: 'GT-Inspector' with: 'GT-Inspector-TudorGirba.371';
			package: 'GT-InspectorExtensions-Core' with: 'GT-InspectorExtensions-Core-YuriyTymchuk.193';
			package: 'GT-Tests-Inspector' with: 'GT-Tests-Inspector-TudorGirba.50'. ].
