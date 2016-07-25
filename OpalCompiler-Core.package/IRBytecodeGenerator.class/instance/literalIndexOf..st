private
literalIndexOf: object

	| index |
	
	(index := literals literalIndexOf: object ifAbsent: 0) > 0 ifFalse: [
		index := self addLiteral: object].
	
	"conversion from 1 based to 0 based"
	^ index - 1