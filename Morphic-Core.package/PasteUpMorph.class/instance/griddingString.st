gridding
griddingString
	"Answer a string to use in a menu offering the user the 
	opportunity to start or stop using gridding"
	^ (self griddingOn
		ifTrue: ['<yes>']
		ifFalse: ['<no>'])
		, 'use gridding' translated