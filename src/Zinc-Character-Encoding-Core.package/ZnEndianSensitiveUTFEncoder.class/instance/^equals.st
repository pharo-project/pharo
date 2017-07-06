comparing
= anObject
	^ super = anObject and: [ self endianness == anObject endianness ]