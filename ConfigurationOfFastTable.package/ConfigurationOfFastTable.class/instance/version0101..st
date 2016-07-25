versions
version0101: spec
	<version: '0.10.1' imports: #('0.3-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'version 0.10.1'.
		spec author: 'EstebanLorenzano'.
		spec timestamp: '2016-03-07 17:10'.
		spec 
			package: 'Morphic-Widgets-FastTable' with: 'Morphic-Widgets-FastTable-EstebanLorenzano.163';
			package: 'Glamour-FastTable' with: 'Glamour-FastTable-TudorGirba.56' ].
