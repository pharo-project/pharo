Morphic-Base-Windows
closeUnchangedWindows
	"Present a menu of window titles for all windows with changes,
	and activate the one that gets chosen."
	(self confirm:
'Do you really want to close all windows
except those with unaccepted edits?' translated)
		ifFalse: [^ self].

	self  closeAllUnchangedWindows.