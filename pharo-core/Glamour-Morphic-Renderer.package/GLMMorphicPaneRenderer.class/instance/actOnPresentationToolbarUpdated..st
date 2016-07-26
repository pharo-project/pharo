rendering
actOnPresentationToolbarUpdated: anAnnouncement
	"If this method is called contentMorph should be an tab widget"
	| presentation |
	
	presentation := anAnnouncement presentation.	
	contentMorph updatePageAtIndex: 1
			withLabel: (self titleOrIconOf: presentation in: contentMorph)
			toolbar: (self renderToolbarForPresentation: presentation inMorph: contentMorph)
			action: (self labelActionBrickFor: presentation)
	
	