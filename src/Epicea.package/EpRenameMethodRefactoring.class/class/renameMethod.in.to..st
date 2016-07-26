instance creation
renameMethod: oldSelector in: affectedClassName to: newSelector
	^ self basicNew
		initializeFrom: oldSelector 
		to: newSelector 
		in: affectedClassName;
		yourself