versions
version31: spec
	<version: '3.1' imports: #('3.0-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'Updated GTools: 
  - bug fixes
  - deprecated PluggableTextMorph
  - improved event recorder'.
		spec author: 'AndreiChis'.
		spec timestamp: '11/9/2015 14:32'.
		spec 
			project: 'GTPlaygroundCore' with: '2.2';
			project: 'GTInspectorCore' with: '2.1';
			project: 'GTSpotter' with: '1.3'. ].
