examples
openWithBorderOfWidth: anInteger
	| st |
	st := self new newScrolledTextWithBorderOfWidth: anInteger.
	self spyAnnouncer ifTrue: [st textArea announcer open].
 	st openInWorld
