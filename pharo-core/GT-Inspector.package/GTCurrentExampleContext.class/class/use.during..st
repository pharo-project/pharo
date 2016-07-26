evaluating
use: anObject during: aBlock
	^ aBlock 
		on: self 
		do: [ :notification | notification resume: anObject ]