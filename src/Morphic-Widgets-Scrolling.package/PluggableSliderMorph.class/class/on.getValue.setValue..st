as yet unclassified
on: anObject getValue: getSel setValue: setSel
	"Answer a new instance of the receiver with
	the given selectors as the interface."

	^self new
		on: anObject
		getValue: getSel
		setValue: setSel