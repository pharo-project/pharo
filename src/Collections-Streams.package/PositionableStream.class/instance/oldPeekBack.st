accessing
oldPeekBack
	"Return the element at the previous position, without changing position.  Use indirect messages in case self is a StandardFileStream."
	"The method is a misconception about what a stream is. A stream contains a pointer *between* elements with past and future elements. This method considers that the pointer is *on* an element. Please consider unit tests which verifies #peekBack and #oldPeekBack behavior. (Damien Cassou - 1 August 2007)"
	| element |
	element := self oldBack.
	self skip: 1.
	^ element
