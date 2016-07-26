versions
version315: spec
	<version: '3.15' imports: #('2.0-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'GToolkit Version 3.23'.
		spec author: 'AndreiChis'.
		spec timestamp: '6/27/2016 12:09'.
		spec project: 'GlamourCore' with: '4.17'.
		spec 
			package: 'GT-Inspector' with: 'GT-Inspector-AndreiChis.432';
			package: 'GT-InspectorExtensions-Core' with: 'GT-InspectorExtensions-Core-TudorGirba.222';
			package: 'GT-Tests-Inspector' with: 'GT-Tests-Inspector-StefanReichhart.56'. ].
