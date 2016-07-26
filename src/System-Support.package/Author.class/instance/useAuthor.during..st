testing-support
useAuthor: aString during: aBlock
	| previous |
	previous := fullName.
	fullName := aString.
	^ aBlock ensure: [ fullName := previous ]