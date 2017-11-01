"
An AbstractTextChangedPlugin is a plugin which simply log each time the method displayer changed
"
Class {
	#name : #AbstractTextChangedPlugin,
	#superclass : #AbstractNautilusPlugin,
	#category : 'NautilusCommon-Plugin'
}

{ #category : #private }
AbstractTextChangedPlugin >> displayerChanged: anAnnouncement [

	self logCr: anAnnouncement displayerSymbol
]

{ #category : #registration }
AbstractTextChangedPlugin >> registerTo: aModel [

	aModel announcer
		when: NautilusTextDisplayerChanged send: #displayerChanged: to: self
]
