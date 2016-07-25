instance creation
rename: oldString to: newString 
	^ self new
		rename: oldString to: newString;
		yourself