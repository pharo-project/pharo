versions
version04: spec
	<version: '0.4' imports: #('0.2-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'FastTable:
- added FTEasyListMorph to create easy list components. 
- added drag and drop support.
- fixed: cells cliping niside table (thanks Nicolai)
- fixed: a bug who was causing bad refreshig of lists when there was no row selected.
- fixed: a search bug who was showing up when there was spaces in strings (like the class list in nautilus)
- fixed: a bug who caused scroll bars to not be resized/positioned correctly when refreshing.
- fixed: search now does not respond to keys if anymodifier is present

Pluggable Adaptor:
- add double click
- fixed a bug causing list not scroll to a selected row (adaptor)'.
		spec author: 'EstebanLorenzano'.
		spec timestamp: '6/3/2015 17:31'.
		spec package: #'Morphic-Widgets-FastTable' with: 'Morphic-Widgets-FastTable-EstebanLorenzano.18'. ].
