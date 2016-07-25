instance creation
referencesToInstanceVariable: aString in: aClass 
	^ self
		on: self default 
		referencesToInstanceVariable: aString 
		in: aClass 