versions
version073: spec
	<version: '0.7.3' imports: #('0.2-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: '- two small changes for making second selection work on Nautilus Protocol list.
- fix an update problem in the adaptor
- add navigation key handling (to allow tab and shift+tab to work)
- minor fix to scrollbar visibility (click on visible scrollbar while scrollbar interval == 1 gave DNU -> make sure scrollbar is hidden if all elements are visible).
- enhanced selection when searching'.
		spec author: 'EstebanLorenzano'.
		spec timestamp: '7/6/2015 12:34'.
		spec package: #'Morphic-Widgets-FastTable' with: 'Morphic-Widgets-FastTable-EstebanLorenzano.37'. ].
