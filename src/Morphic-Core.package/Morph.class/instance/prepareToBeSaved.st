copying
prepareToBeSaved
	"Prepare this morph to be saved to disk. Subclasses should nil out any instance variables that holds state that should not be saved, such as cached Forms. Note that this operation may take more drastic measures than releaseCachedState; for example, it might discard the transcript of an interactive chat session."

	self releaseCachedState.
	self formerOwner: nil.
	self formerPosition: nil.
	fullBounds := nil