versions
version071: spec
	<version: '0.7.1' imports: #('0.2-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: '- fixed the show bottom issue (or at least, I *think* is fixed) :)
- fixed an error preventing select already selected issues when allowsDeselection = false'.
		spec author: 'EstebanLorenzano'.
		spec timestamp: '6/24/2015 15:05'.
		spec package: #'Morphic-Widgets-FastTable' with: 'Morphic-Widgets-FastTable-EstebanLorenzano.31'. ].
