versions
version011: spec
	<version: '0.1.1' imports: #('0.1-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'better handling of exceptions when trying to install'.
		spec author: 'EstebanLorenzano'.
		spec timestamp: '5/4/2015 22:43'.
		spec project: 'NeoJSON' with: '10'.
		spec package: #'Tool-Catalog' with: 'Tool-Catalog-EstebanLorenzano.2'. ].
