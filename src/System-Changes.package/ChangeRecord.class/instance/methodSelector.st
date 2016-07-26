access
methodSelector
	^ type == #method
		ifTrue: [ (Smalltalk globals at: class ifAbsent: [ Object ]) compiler parseSelector: self string ]