actions
addPageIfInteresting: aPage
	aPage isInteresting ifFalse: [ ^ self ].
	self addPage: aPage