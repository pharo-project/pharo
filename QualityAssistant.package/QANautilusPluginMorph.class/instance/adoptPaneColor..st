accessing
adoptPaneColor: paneColor
	super adoptPaneColor: paneColor.
	
	"I have no idea how to solve this in a different way.
	 The list border color is changed throught this method,
	 I have to set progress morphs border to be the same"
	progressMorph borderStyle:
		(Smalltalk ui theme listNormalBorderStyleFor: listMorph).