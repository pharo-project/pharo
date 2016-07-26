initialization
initializePresenter

	viewer whenClassChanged: [ :class | text behavior: class ].

	viewer whenProtocolChangedDo: [ :item | 
		item 
			ifNil: [ text text: '' ]
			ifNotNil: [ text text: item sourceCode ] ].
	viewer whenEventChangedDo: [ :item | 
		item 
			ifNil: [ text text: '' ]
			ifNotNil: [ text text: item sourceCode ] ]