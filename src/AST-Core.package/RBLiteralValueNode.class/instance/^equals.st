comparing
= anObject 
	^ super = anObject 
		and: [ self value = anObject value 
		and: [ self value species = anObject value species ] ]