comparing
= aPDPackageCycle
	(aPDPackageCycle isKindOf: self class) ifFalse: [ ^ false ]. 
	^ self size = aPDPackageCycle size and: [ self packages includesAll: aPDPackageCycle packages ].