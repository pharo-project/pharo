accessing
initializeWithTree: expr
	expr isSymbol
		ifTrue: [name := expr.
				children := Array new.
				hasResolution := true.]
		ifFalse: [name := expr first.
				expr second isSymbol
					ifTrue: [hasResolution := false.
							children := Array new]
					ifFalse: [hasResolution := true.
							children := expr second]]