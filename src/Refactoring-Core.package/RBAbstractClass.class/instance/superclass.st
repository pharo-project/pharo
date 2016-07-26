accessing
superclass
	^superclass == LookupSuperclass 
		ifTrue: [model classFor: self realClass superclass]
		ifFalse: [superclass]