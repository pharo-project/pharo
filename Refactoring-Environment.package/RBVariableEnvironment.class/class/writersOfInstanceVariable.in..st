instance creation
writersOfInstanceVariable: aString in: aClass 
	^ self
		on: self default
		writersOfInstanceVariable: aString 
		in: aClass 