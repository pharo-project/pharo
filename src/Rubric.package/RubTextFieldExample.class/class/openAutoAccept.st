examples
openAutoAccept
	| st model |
	model := self new.
	st := model newScrolledTextAutoAccept.
	model inspect.
	self spyAnnouncer ifTrue: [st textArea announcer open].
 	st openInWorld
