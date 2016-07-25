converting
withoutPeriodSuffix
	"Return a copy of the receiver up to, but not including, the first period.  If the receiver's *first* character is a period, then just return the entire receiver. "

	| likely |
	likely := self copyUpTo: $..
	^ likely size = 0
		ifTrue: [ self ]
		ifFalse: [ likely ]