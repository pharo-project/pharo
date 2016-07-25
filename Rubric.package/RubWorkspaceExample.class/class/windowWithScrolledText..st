private - utilities
windowWithScrolledText: aScrolledText 
	| w |
	w := StandardWindow new.
	w title: self title.
	self spyAnnouncer ifTrue: [aScrolledText textArea announcer open].
	w addMorph: aScrolledText fullFrame: (0@0 corner: 1@1) asLayoutFrame.
	^w