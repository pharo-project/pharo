running
basicCheck: anEntity
	"this has to be rewritten in a smarter way, when the complete implementation with critics will be ready".
	
	^ (self leaves select: [ :rule | rule class perform: (
		anEntity isCompiledMethod
			ifTrue: [ #checksMethod ]
			ifFalse: [ #checksClass ]) ]) anySatisfy: [ :rule |
				rule basicCheck: anEntity ]