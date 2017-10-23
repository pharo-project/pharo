construction
add: aString action: aMessageSend
	| item |
	item := self addMenuItem.
	item label: aString translated.
	item action: aMessageSend.
	^item