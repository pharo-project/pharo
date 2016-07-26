versions
version32: spec
	<version: '3.2' imports: #('2.0-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'GTools version 3.13'.
		spec author: 'AndreiChis'.
		spec timestamp: '2/23/2016 17:56'.
		spec project: 'GlamourCore' with: '4.3'.
		spec 
			package: 'GT-Inspector' with: 'GT-Inspector-TudorGirba.382';
			package: 'GT-InspectorExtensions-Core' with: 'GT-InspectorExtensions-Core-AndreiChis.203';
			package: 'GT-Tests-Inspector' with: 'GT-Tests-Inspector-AndreiChis.53'. ].
