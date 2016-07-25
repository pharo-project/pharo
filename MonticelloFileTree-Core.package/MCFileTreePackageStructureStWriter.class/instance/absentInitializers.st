accessing
absentInitializers
	^ initializers reject: [ :each | Smalltalk hasClassNamed: each key ]