instance creation
rename: oldName to: newName in: aClass 
	^ self new
		oldName: oldName;
		newName: newName;
		changeClass: aClass;
		yourself