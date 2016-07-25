accessing
at: key ifAbsentPut: aBlock 
 	"Answer the value associated with the key or, if key isn't found,
 	add the result of evaluating aBlock to self"
 
 	^ self at: key ifAbsent: [self add: aBlock value]