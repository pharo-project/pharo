listing
visibleProtocols
	| methods protocols |
	self switchIsComment ifTrue: [^ Array new].
	methods := self methodsForSelectedClass.
	protocols := (methods collect: [:ea | ea category]) asSet asSortedCollection.
	protocols add: AllProtocol defaultName.
	^ protocols 