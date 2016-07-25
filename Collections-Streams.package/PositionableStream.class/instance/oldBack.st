accessing
oldBack
	"Go back one element and return it.  Use indirect messages in case I am a StandardFileStream"
	"The method is a misconception about what a stream is. A stream contains a pointer *between* elements with past and future elements. This method considers that the pointer is *on* an element. Please consider unit tests which verifies #back and #oldBack behavior. (Damien Cassou - 1 August 2007)"
	self position = 0 ifTrue: [self positionError].
	self position = 1 ifTrue: [self position: 0.  ^ nil].
	self skip: -2.
	^ self next
