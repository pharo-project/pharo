versions
version0102: spec
	<version: '0.10.2' imports: #('0.3-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'version 0.10.2'.
		spec author: 'CyrilFerlicot'.
		spec timestamp: '4/18/2016 11:31'.
		spec 
			package: #'Morphic-Widgets-FastTable' with: 'Morphic-Widgets-FastTable-StephanEggermont.177';
			package: 'Glamour-FastTable' with: 'Glamour-FastTable-AndreiChis.59'. ].
