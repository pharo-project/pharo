accessing
getComments
	| oldComments |
	comments isEmpty ifTrue: [^nil].
	oldComments := comments.
	comments := OrderedCollection new: 1.
	^oldComments