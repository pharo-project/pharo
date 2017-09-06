accessing
forApproxMessagesOf: aMethod do: aBlock
	"this is so ugly purely for performance reasons"
	"as oposed to the #messages method of the CompiledCode class
	this approach is 11 times faster and had 87% precision and
	100% recall in the standard pharo image upon method creation"

	"literals are shifted by 1 right (that's why we start from 2"
	"2 last literalls are method name and class name
	(that's why we go to num - 1 (had to be -2 but mind the shift))"
	2 to: aMethod numLiterals - 1 do: [ :i |
		| l |
		l := aMethod objectAt: i.
		"besides symbols literals may be associations for class reffs
		or other types of the actual literals"
		l isSymbol ifTrue: [ aBlock value: l ] ]