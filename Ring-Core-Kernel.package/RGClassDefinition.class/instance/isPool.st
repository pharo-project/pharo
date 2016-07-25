managing pool users
isPool
	"The receiver is a shared pool if it inherits from SharedPool"
	
	^self annotationNamed: self class isPoolKey 
		ifAbsent: [ self superclassName == #SharedPool ]