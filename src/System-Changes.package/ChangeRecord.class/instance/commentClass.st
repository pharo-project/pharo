access
commentClass 
	| commentClass |
	type == #classComment ifFalse: [^ nil].
	(Smalltalk includesKey: class asSymbol) ifFalse: [^ nil].
	commentClass := Smalltalk at: class asSymbol.
	^meta ifTrue: [commentClass class]
		ifFalse: [commentClass]
