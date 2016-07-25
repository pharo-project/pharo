accessing
changeClass
	^ Smalltalk globals at: oldName asSymbol ifAbsent: [ Smalltalk globals at: newName asSymbol ]