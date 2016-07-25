instance creation
rename: oldName to: newName in: affectedClassName
	^ self basicNew 
		initializeIn: affectedClassName
		from: oldName 
		to: newName;
		yourself