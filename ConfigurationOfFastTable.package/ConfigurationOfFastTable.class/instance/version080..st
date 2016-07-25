versions
version080: spec
	<version: '0.8.0' imports: #('0.3-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'version 0.8.0'.
		spec author: 'CyrilFerlicot'.
		spec timestamp: '11/17/2015 10:53'.
		spec 
			package: #'Morphic-Widgets-FastTable' with: 'Morphic-Widgets-FastTable-usmanbhatti.119';
			package: 'Glamour-FastTable' with: 'Glamour-FastTable-usmanbhatti.41'. ].
