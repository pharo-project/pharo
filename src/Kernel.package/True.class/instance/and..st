controlling
and: alternativeBlock 
	"Nonevaluating conjunction -- answer the value of alternativeBlock since
	the receiver is true."
	"
	true and: [ false ]
	>>> false
	"
	"
	true and: [ true ]
	>>> true
	"
	^alternativeBlock value