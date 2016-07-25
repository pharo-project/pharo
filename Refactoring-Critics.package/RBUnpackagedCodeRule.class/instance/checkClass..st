running
checkClass: aClass 
	(self basicCheck: aClass) ifTrue: [
		self result addClass: aClass ]