rendering
actOnMatchingPresentationsChanged: anAnnouncement
	"whenever a relevant pane changes its presentations,
	we override the contents from the GLMWatcherWindow"
	
	GLMWatcherWindow uniqueInstance 
		addContent: [ self renderObject: anAnnouncement pane presentations ]