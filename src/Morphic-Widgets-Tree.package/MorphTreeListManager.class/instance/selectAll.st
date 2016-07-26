selection change
selectAll
	self allNodeMorphs isEmpty
		ifTrue: [^ self].
	self isMultiple
		ifFalse: [^ self]. 
	self addAllToSelection: self allNodeMorphs