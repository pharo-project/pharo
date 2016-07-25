versions
version0711: spec
	<version: '0.7.11' imports: #('0.3-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'version 0.7.11'.
		spec author: 'CyrilFerlicot'.
		spec timestamp: '10/29/2015 15:03'.
		spec 
			package: #'Morphic-Widgets-FastTable' with: 'Morphic-Widgets-FastTable-CyrilFerlicot.100';
			package: 'Glamour-FastTable' with: 'Glamour-FastTable-CyrilFerlicot.19'. ].
