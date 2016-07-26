examples
openScrolledText: aScrolledText

	|  window |
	window := StandardWindow new.
	window addMorph: aScrolledText fullFrame: (0@0 corner: 1@1) asLayoutFrame.
	window title: self title.
	self spyAnnouncer ifTrue: [ aScrolledText textArea announcer open ].
	window openInWorld