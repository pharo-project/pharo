initialize-release
initialize: aStream

	source := aStream.
	lookahead := aStream next.
	elements := OrderedCollection new