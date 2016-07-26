enumerating
reverseDo: aBlock 
	"Evaluate aBlock for each element of my interval, in reverse order.
	Implementation notes: see do: for an explanation on loop detail"

	| aValue index |
	index := self size.
	[index > 0]
		whileTrue: [
			index := index - 1.
			aValue := start + (index * step).
			aBlock value: aValue]