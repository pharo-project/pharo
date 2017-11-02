"
AbstractKeyPressedPlugin is an abstract plugin which reacts to key strokes by sending the message keyPressed to the plugin.
"
Class {
	#name : #AbstractKeyPressedPlugin,
	#superclass : #AbstractNautilusPlugin,
	#category : 'NautilusCommon-Plugin'
}

{ #category : #announcement }
AbstractKeyPressedPlugin >> keyPressed: anAnnouncement [

	self subclassResponsibility
]

{ #category : #registration }
AbstractKeyPressedPlugin >> registerTo: aModel [

	aModel announcer
		when: NautilusKeyPressed send: #keyPressed: to: self
]
