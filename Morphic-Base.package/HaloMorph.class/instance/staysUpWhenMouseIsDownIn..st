events
staysUpWhenMouseIsDownIn: aMorph
	^ ((aMorph == target) or: [aMorph hasOwner: self])