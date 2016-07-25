versions
version215: spec
	<version: '2.1.5' imports: #('2.1-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'version 2.1.5'.
		spec author: 'CyrilFerlicot'.
		spec timestamp: '6/14/2015'.
		
		spec package: 'GT-Playground' with: 'GT-Playground-CyrilFerlicot.111'.
		spec package: 'GT-Tests-Playground' with: 'GT-Tests-Playground-StefanReichhart.4'.
		spec project: 'GTInspectorCore' with: '2.0.15'. ]
	
