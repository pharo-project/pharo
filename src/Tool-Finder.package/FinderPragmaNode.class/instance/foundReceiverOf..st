private
foundReceiverOf: aString
	"It's ugly, but I haven't found a method that allows me to do that easily"

	| selector index firstPart |
	selector := (self model finder findSelector: aString).
	index := selector indexOf: $:.
	firstPart := selector.
	(index = 0)
		ifFalse: [firstPart := selector copyFrom: 1 to: index].
	index :=  aString findString: firstPart.
	^ aString copyFrom: 1 to: (index-1).