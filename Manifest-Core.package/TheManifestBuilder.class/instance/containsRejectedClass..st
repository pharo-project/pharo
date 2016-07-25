manifest
containsRejectedClass: aClass
	
	^ self rejectClasses anySatisfy: [ :cl | cl = aClass ]