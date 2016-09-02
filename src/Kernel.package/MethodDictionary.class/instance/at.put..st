accessing
at: key put: value
	"Set the value at key to be value."
	| index |
	index := self findElementOrNil: key.
	(self basicAt: index) 
		ifNil: 
			[tally := tally + 1.
			self basicAt: index put: key].
	array at: index put: value.
	key flushCache. "flush the vm cache by selector"
	self fullCheck.
	^ value