versions
version014: spec
	<version: '0.14' imports: #('0.6-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'sync with Pharo package versions'.
		spec author: 'PavelKrivanek'.
		spec timestamp: '6/1/2016 11:20'.
		spec project: 'Ston' with: '0.19'.
		spec package: #'Tool-Catalog' with: 'Tool-Catalog-TheIntegrator.34'. ].
