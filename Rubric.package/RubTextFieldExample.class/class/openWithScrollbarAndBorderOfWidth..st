examples
openWithScrollbarAndBorderOfWidth: anInteger
	| st |
	st := self new newScrolledTextWithScrollbarAndBorderOfWidth: anInteger.
	self spyAnnouncer ifTrue: [st textArea announcer open].
 	st openInWorld
