testing
includesSelector: aSelector ofClass: aClass
	"Return true if the receiver includes the method of selector aSelector. Checks methods defined locally as well as extending other packages"
	
	^ aClass isMeta
		ifTrue: [ self includesSelector: aSelector ofMetaclassName: aClass theNonMetaClass name  ]
		ifFalse: [ self includesSelector: aSelector ofClassName: aClass name  ]
	
