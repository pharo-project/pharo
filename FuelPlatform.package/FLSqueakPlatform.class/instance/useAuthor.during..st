accessing-system
useAuthor: aString during: aBlock
	| previous utilities |
	utilities := (Smalltalk at:#Utilities).
	previous := utilities authorInitialsPerSe.
	utilities setAuthorInitials: aString.
	^ aBlock ensure: [ utilities setAuthorInitials: previous ]