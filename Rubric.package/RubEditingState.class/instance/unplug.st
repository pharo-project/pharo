private
unplug
	super unplug.
	undoManager
		ifNotNil: [ 
			undoManager reset.
			undoManager := nil ].
	 pointBlock := markBlock := startOfTyping := previousInterval := nil.