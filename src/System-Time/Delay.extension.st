Extension { #name : 'Delay' }

{ #category : '*System-Time' }
Delay class >> forDuration: aDuration [
 	"Return a new Delay for the given duration."
	self deprecated: 'Use #forMilliseconds: instead'.
 	^ self forMilliseconds: aDuration asMilliSeconds
]
