versions
version314: spec
	<version: '3.14' imports: #('2.0-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'solve circular dependency on Tool-Catalog'.
		spec author: 'PavelKrivanek'.
		spec timestamp: '6/7/2016 16:25'.
		spec project: 'GlamourCore' with: '4.16'.
		spec 
			package: 'GT-Inspector' with: 'GT-Inspector-AndreiChis.421';
			package: 'GT-InspectorExtensions-Core' with: 'GT-InspectorExtensions-Core-PavelKrivanek.218';
			package: 'GT-Tests-Inspector' with: 'GT-Tests-Inspector-StefanReichhart.56'. ].
