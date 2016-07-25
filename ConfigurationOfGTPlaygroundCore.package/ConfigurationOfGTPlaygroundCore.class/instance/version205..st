versions
version205: spec
	<version: '2.0.5' imports: #('2.0-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'version 2.0.5'.
		spec author: 'AndreiChis'.
		spec timestamp: '12/10/2014 13:38'.
		
		spec package: 'GT-Playground' with: 'GT-Playground-AndreiChis.53'.
		spec project: 'GTInspectorCore' with: '2.0.5'. ]
