accessing
presentInitializers
	^ initializers select: [ :each | Smalltalk hasClassNamed: each key ]