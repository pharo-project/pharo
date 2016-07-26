examples
open
	| st |
	st := self new newScrolledText.
	self spyAnnouncer ifTrue: [st textArea announcer open].
 	st openInWorld
