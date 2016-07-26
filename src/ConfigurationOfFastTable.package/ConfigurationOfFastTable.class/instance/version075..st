versions
version075: spec
	<version: '0.7.5' imports: #('0.2-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: '- bugfix: infinite loop when drawing because of a invalidate caused for a morph owner change
- adaptor responds correct transfer type, allowing drag&drop'.
		spec author: 'EstebanLorenzano'.
		spec timestamp: '8/6/2015 17:01'.
		spec package: #'Morphic-Widgets-FastTable' with: 'Morphic-Widgets-FastTable-EstebanLorenzano.40'. ].
