refactoring
sourceTextArea
	^ model window submorphs detect: [:ea | ea respondsTo: #getTextSelector ].