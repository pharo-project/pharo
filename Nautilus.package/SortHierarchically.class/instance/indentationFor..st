computation
indentationFor: anElement
	"Return the number of spaces needed before anElement to indent it properly."
	"Speed could be enhanced by using a hash-based collection instead of this #detect:."
	^ (hierarchy detect: [ :node | node element = anElement ]) indent