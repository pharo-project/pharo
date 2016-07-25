actions
comment: comment
	| commentString |
	commentString := comment asString trimBoth.
	commentString ifEmpty: [ ^ self ].
	self unit comment: commentString