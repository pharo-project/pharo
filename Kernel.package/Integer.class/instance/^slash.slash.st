arithmetic
// aNumber 
	| q |
	aNumber = 0 ifTrue: [^ (ZeroDivide dividend: self) signal"<- Chg"].
	self = 0 ifTrue: [^ 0].
	q := self quo: aNumber.
	"Refer to the comment in Number|//."
	(q negative
		ifTrue: [q * aNumber ~= self]
		ifFalse: [q = 0 and: [self negative ~= aNumber negative]])
		ifTrue: [^ q - 1"Truncate towards minus infinity."]
		ifFalse: [^ q]