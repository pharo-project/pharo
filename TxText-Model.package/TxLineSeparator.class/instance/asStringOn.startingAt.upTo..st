printing
asStringOn: aStream startingAt: startIndex upTo: endIndex
	"specialized in subclasses"
	(endIndex - startIndex > 0) ifTrue: [ 
		aStream cr.
		 ]