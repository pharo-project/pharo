comparing
= other
	super = other
		ifFalse: [ ^ false ].
	
	self size = other size
		ifFalse: [ ^ false ].
	
	1 to: self size do:  [ :index|
		(self at: index) = (other at: index)
			ifFalse: [ ^ false ]].
	
	^ self parentScope = other parentScope.