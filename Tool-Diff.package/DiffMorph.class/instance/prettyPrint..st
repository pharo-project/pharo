accessing
prettyPrint: aBoolean
	"Set the value of prettyPrint"

	prettyPrint == aBoolean ifTrue: [^self].
	prettyPrint := aBoolean.
	self updateText
	