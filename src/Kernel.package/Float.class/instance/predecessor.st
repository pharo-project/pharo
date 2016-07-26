truncation and round off
predecessor
	| ulp |
	self isFinite ifFalse: [
		(self isNaN or: [self negative]) ifTrue: [^self].
		^Float fmax].
	self = 0.0 ifTrue: [^Float fmin negated].
	ulp := self ulp.
	^self - (0.5 * ulp) = self
		ifTrue: [self - ulp]
		ifFalse: [self - (0.5 * ulp)]