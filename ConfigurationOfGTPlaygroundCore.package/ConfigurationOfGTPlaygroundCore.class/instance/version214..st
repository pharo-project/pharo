versions
version214: spec
	<version: '2.1.4' imports: #('2.1-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'version 2.1.4'.
		spec author: 'AndreiChis'.
		spec timestamp: '5/15/2015'.
		
		spec package: 'GT-Playground' with: 'GT-Playground-TudorGirba.110'.
		spec package: 'GT-Tests-Playground' with: 'GT-Tests-Playground-StefanReichhart.4'.
		spec project: 'GTInspectorCore' with: '2.0.15'. ]
	
