accessing
selectedClass
	self className ifNil: [ ^ nil ]. 
	^ Smalltalk globals at: self className ifAbsent: []