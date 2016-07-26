accessing
identityIndexOf: anElement ifAbsent: exceptionBlock
	"Answer the index of anElement within the receiver. If the receiver does 
	not contain anElement, answer the result of evaluating the argument, 
	exceptionBlock."
	1 to: self size do:
		[:i | (self at: i) == anElement ifTrue: [^ i]].
	^ exceptionBlock value