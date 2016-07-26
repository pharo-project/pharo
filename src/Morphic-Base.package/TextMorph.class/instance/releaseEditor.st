private
releaseEditor 
	"Release the editor for my paragraph.  This morph no longer 'hasFocus'."
	editor ifNotNil:
		[self selectionChanged.
		self paragraph selectionStart: nil selectionStop: nil.
		editor := nil].