instance creation
fromString: aString
	| text |
	text := self new.
	text endPosition insert: aString.
	^ text