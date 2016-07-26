versions
version310: spec
	<version: '3.10' imports: #('2.0-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'version 3.10'.
		spec author: 'GuillermoPolito'.
		spec timestamp: '4/25/2016 16:54'.
		spec project: 'GlamourCore' with: '4.12'.
		spec 
			package: 'GT-Inspector' with: 'GT-Inspector-GuillermoPolito.407';
			package: 'GT-InspectorExtensions-Core' with: 'GT-InspectorExtensions-Core-TudorGirba.213';
			package: 'GT-Tests-Inspector' with: 'GT-Tests-Inspector-AndreiChis.53'. ].
