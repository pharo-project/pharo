versions
version082: spec
	<version: '0.8.2' imports: #('0.3-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'version 0.8.2'.
		spec author: 'CyrilFerlicot'.
		spec timestamp: '1/11/2016 11:04'.
		spec 
			package: #'Morphic-Widgets-FastTable' with: 'Morphic-Widgets-FastTable-CyrilFerlicot.147';
			package: 'Glamour-FastTable' with: 'Glamour-FastTable-CyrilFerlicot.47'. ].
