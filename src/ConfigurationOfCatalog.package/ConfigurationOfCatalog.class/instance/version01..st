versions
version01: spec
	<version: '0.1' imports: #('0.1-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'version bump. 
works, but requires NeoJSON and feels incomplete.'.
		spec author: 'EstebanLorenzano'.
		spec timestamp: '5/4/2015 21:39'.
		spec project: 'NeoJSON' with: #'stable'.
		spec package: #'Tool-Catalog' with: 'Tool-Catalog-EstebanLorenzano.1'. ].
