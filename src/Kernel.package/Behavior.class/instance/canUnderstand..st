testing method dictionary
canUnderstand: selector
	"Answer whether the receiver can respond to the message whose selector 
	is the argument."

	^ self
		classAndMethodFor: selector
		do: [ :c :m | true ]
		ifAbsent: [ false ].