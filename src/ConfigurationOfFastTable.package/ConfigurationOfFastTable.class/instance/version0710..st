versions
version0710: spec
	<version: '0.7.10' imports: #('0.3-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'version 0.7.10'.
		spec author: 'CyrilFerlicot'.
		spec timestamp: '10/29/2015 10:07'.
		spec 
			package: #'Morphic-Widgets-FastTable' with: 'Morphic-Widgets-FastTable-CyrilFerlicot.98';
			package: 'Glamour-FastTable' with: 'Glamour-FastTable-CyrilFerlicot.19'. ].
