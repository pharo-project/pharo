display
display
	"Nautilus calls this method when the plugin is first displayed. Show the Finder bar."
	finder := Finder new.
	
	finder
		when: #updateResultDictionary
		send: #openFinderUI
		to: self.
		
	finderui := FinderUI on: finder.
	
	^ (finderui buildSearchModeToolBar: StandardWindow new) height: 25