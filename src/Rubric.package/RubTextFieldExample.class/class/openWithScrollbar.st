examples
openWithScrollbar
	| st |
	st := self new newScrolledTextWithScrollbar.
	self spyAnnouncer ifTrue: [st textArea announcer open].
 	st openInWorld
