accessing
peekFor: item 
	"Answer false and do not advance if the next element is not equal to item, or if this stream is at the end.  If the next element is equal to item, then advance over it and return true"
	| next |
	(next := self next) == nil ifTrue: [^ false].
	item = next ifTrue: [^ true].
	self skip: -1.
	^ false