versions
version07: spec
	<version: '0.7' imports: #('0.2-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: '- refactored data sources to keep owner as instance variable 
- added allowDelection (seems used and useful)
- drag&drop receive drag type as parameter
- added enable/disable search
- added resize of columns on table (needed when having more than one column)
- some fixes in adaptor to respond better to pluggable list usages.'.
		spec author: 'EstebanLorenzano'.
		spec timestamp: '6/18/2015 16:17'.
		spec package: #'Morphic-Widgets-FastTable' with: 'Morphic-Widgets-FastTable-EstebanLorenzano.30'. ].
