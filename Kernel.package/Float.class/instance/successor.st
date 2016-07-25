truncation and round off
successor
	| ulp |
	self isFinite ifFalse: [
		(self isNaN or: [self positive]) ifTrue: [^self].
		^Float fmax negated].
	self = 0.0 ifTrue: [^Float fmin].
	ulp := self ulp.
	^self + (0.5 * ulp) = self
		ifTrue: [self + ulp]
		ifFalse: [self + (0.5 * ulp)]