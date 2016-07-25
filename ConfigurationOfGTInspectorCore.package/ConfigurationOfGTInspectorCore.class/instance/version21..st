versions
version21: spec
	<version: '2.1' imports: #('2.0-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'Updated GTools: 
  - bug fixes
  - deprecated PluggableTextMorph
  - improved event recorder'.
		spec author: 'AndreiChis'.
		spec timestamp: '11/9/2015 14:32'.
		spec project: 'GlamourCore' with: '3.3'.
		spec 
			package: 'GT-Inspector' with: 'GT-Inspector-AndreiChis.347';
			package: 'GT-InspectorExtensions-Core' with: 'GT-InspectorExtensions-Core-AndreiChis.185';
			package: 'GT-Tests-Inspector' with: 'GT-Tests-Inspector-StefanReichhart.41'. ].
