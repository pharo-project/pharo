*MonticelloGUI
defaultAction
	^ MCSaveVersionDialog new
		versionName: suggestion;
		logMessage: suggestedLogComment;
		showModally