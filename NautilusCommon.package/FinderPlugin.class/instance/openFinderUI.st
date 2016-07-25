display
openFinderUI
	"The finder bar calls this method when the user accepts his search query."
	finderui open.
	
	"Without that, if you reuse the same Finder for anotehr research, it will open a new window again"
	finder
		removeActionsWithReceiver: self