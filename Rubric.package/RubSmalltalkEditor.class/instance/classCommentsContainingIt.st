menu messages
classCommentsContainingIt
	"Open a browser class comments which contain the current selection somewhere in them."

	self lineSelectAndEmptyCheck: [^ self].
	self model systemNavigation  browseClassCommentsWithString: self selection string matchCase: false. 