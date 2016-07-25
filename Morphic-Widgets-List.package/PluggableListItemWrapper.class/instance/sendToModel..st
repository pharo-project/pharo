private
sendToModel: aSelector
	aSelector numArgs = 0 
		ifTrue:[^model perform: aSelector].
	aSelector numArgs = 1 
		ifTrue:[^model perform: aSelector with: item].
	aSelector numArgs = 2 
		ifTrue:[^model perform: aSelector with: item with: self].