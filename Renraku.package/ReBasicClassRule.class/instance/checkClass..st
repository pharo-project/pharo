as yet unclassified
checkClass: aClass

	(self basicCheck: aClass) ifTrue: 
		[ result addClass: aClass ]