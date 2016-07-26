private-parsing
saveCommentsDuring: aBlock
	| currentComments |
	currentComments := comments copy.
	comments removeAll.
	^ aBlock
		ensure: [ comments := comments, currentComments ]