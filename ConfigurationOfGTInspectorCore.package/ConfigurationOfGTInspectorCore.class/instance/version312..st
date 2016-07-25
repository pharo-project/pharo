versions
version312: spec
	<version: '3.12' imports: #('2.0-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'GToolkit version 3.21 (case 18112)'.
		spec author: 'AndreiChis'.
		spec timestamp: '4/27/2016 19:09'.
		spec project: 'GlamourCore' with: '4.13'.
		spec 
			package: 'GT-Inspector' with: 'GT-Inspector-AndreiChis.418';
			package: 'GT-InspectorExtensions-Core' with: 'GT-InspectorExtensions-Core-TheIntegrator.214';
			package: 'GT-Tests-Inspector' with: 'GT-Tests-Inspector-StefanReichhart.56'. ].
