versions
version313: spec
	<version: '3.13' imports: #('2.0-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'GToolkit version 3.22 (case 17948)'.
		spec author: 'AndreiChis'.
		spec timestamp: '4/28/2016 16:25'.
		spec project: 'GlamourCore' with: '4.15'.
		spec 
			package: 'GT-Inspector' with: 'GT-Inspector-AndreiChis.421';
			package: 'GT-InspectorExtensions-Core' with: 'GT-InspectorExtensions-Core-TheIntegrator.214';
			package: 'GT-Tests-Inspector' with: 'GT-Tests-Inspector-StefanReichhart.56'. ].
