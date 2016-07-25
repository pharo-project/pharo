accessing
occupant: anObject 
	"anObject sends me data. I weakly keeps anObject and once it is released, I can be removed from GTEventRecorder object."
	self registry object: anObject