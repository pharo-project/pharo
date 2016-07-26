lookup
newMethodScope
	 
	^ OCMethodScope new outerScope: (self outerScope: outerScope instanceScope) 