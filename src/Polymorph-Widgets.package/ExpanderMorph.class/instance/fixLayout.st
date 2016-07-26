operations
fixLayout
	"Fix the owner layout, nasty!"

	self owner ifNil: [ ^ self ].
	self owner
		allMorphsDo: [ :m | 
			(m respondsTo: #resetExtent)
				ifTrue: [ 
					self defer: (MessageSend receiver: m selector: #resetExtent).
					self defer: (MessageSend receiver: m selector: #setScrollDeltas) ].
			(m isKindOf: self class)
				ifTrue: [ self defer: (MessageSend receiver: m selector: #adoptPaneColor) ] ].
	self defer: (MessageSend receiver: self owner selector: #changed)