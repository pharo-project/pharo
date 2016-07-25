initialization
initializePresenter
	"initialize the event handlings"

	super initializePresenter.
	description
		whenTextIsAccepted: [ :newText | 
			self saveElement: newText.
			self refreshDescription ]