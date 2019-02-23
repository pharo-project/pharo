"
This is a special widget renderer in that it will get to render multiple panes on the same container morph provided by a unique instance of the GLMWatcherWindow.
"
Class {
	#name : #GLMMorphicWatcherRenderer,
	#superclass : #GLMMorphicWidgetRenderer,
	#category : #'Glamour-Morphic-Renderer'
}

{ #category : #rendering }
GLMMorphicWatcherRenderer >> actOnMatchingPresentationsChanged: anAnnouncement [
	"whenever a relevant pane changes its presentations,
	we override the contents from the GLMWatcherWindow"
	
	GLMWatcherWindow uniqueInstance 
		addContent: [ self renderObject: anAnnouncement pane presentations ]
]

{ #category : #rendering }
GLMMorphicWatcherRenderer >> render: aPane [
	"we just register for the announcement"

	aPane when: GLMMatchingPresentationsChanged send: #actOnMatchingPresentationsChanged: to: self
]
