instance creation
readersOfInstanceVariable: aString in: aClass 
	^ self
		on: self default
		readersOfInstanceVariable: aString
		in: aClass