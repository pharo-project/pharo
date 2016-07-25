copying
postCopy
	| newDict |
	super postCopy.
	newDict := classSelectors copy.
	newDict keysAndValuesDo: [:key :value | newDict at: key put: value copy].
	classSelectors := newDict.
	newDict := metaClassSelectors copy.
	newDict keysAndValuesDo: [:key :value | newDict at: key put: value copy].
	metaClassSelectors := newDict