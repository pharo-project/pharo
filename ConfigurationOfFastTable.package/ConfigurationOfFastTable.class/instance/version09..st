versions
version09: spec
	<version: '0.9' imports: #('0.3-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'add resizable columns'.
		spec author: 'EstebanLorenzano'.
		spec timestamp: '1/22/2016 14:56'.
		spec 
			package: #'Morphic-Widgets-FastTable' with: 'Morphic-Widgets-FastTable-EstebanLorenzano.161';
			package: 'Glamour-FastTable' with: 'Glamour-FastTable-CyrilFerlicot.52'. ].
