initialization
initializeWidgets

	text := self newText.
		
	text whenBuiltDo: [ :builtAnnouncement | 
		text whenScrollValueChanged: [ :newPosition | self syncMorphPosition ] ].
	
	text text: self loremIpsum.