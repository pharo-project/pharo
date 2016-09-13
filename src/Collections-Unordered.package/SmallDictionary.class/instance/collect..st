enumerating
collect: aBlock 
	"Evaluate aBlock with each of my values as the argument.  Collect the
	resulting values into a collection that is like me. Answer with the new
	collection."
	| newCollection |
	newCollection := self species new.
	self associationsDo:[:each |
		newCollection at: each key put: (aBlock value: each value).
	].
	^newCollection