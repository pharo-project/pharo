converting
asOrderedCollection 

	self species == OrderedCollection ifTrue: [ ^self ].
	^super asOrderedCollection 