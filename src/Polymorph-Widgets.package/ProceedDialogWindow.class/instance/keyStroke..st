as yet unclassified
keyStroke: evt
	"Additionally check for y and n keys (aliases for ok and cancel)."

	(super keyStroke: evt) ifTrue: [^true].
	evt keyCharacter = $y ifTrue: [self yes. ^true].
	evt keyCharacter = $n ifTrue: [self no. ^true].
	^false