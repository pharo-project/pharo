controlling
and: alternativeBlock 
	"Nonevaluating conjunction -- answer with false since the receiver is false."
	"
	false and: [ 1 error ]
	>>> false
	"
	
	^self