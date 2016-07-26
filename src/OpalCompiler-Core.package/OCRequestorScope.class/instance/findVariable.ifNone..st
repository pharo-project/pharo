lookup
findVariable: aBlock ifNone: aNotFound
	outerScope ifNotNil: [
		^  outerScope findVariable: aBlock ifNone: aNotFound ]