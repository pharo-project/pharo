versions
version081: spec
	<version: '0.8.1' imports: #('0.3-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'version 0.8.1'.
		spec author: 'CyrilFerlicot'.
		spec timestamp: '12/8/2015 10:30'.
		spec 
			package: #'Morphic-Widgets-FastTable' with: 'Morphic-Widgets-FastTable-CyrilFerlicot.142';
			package: 'Glamour-FastTable' with: 'Glamour-FastTable-CyrilFerlicot.42'. ].
