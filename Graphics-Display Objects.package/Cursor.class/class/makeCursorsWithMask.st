initialization - private
makeCursorsWithMask
	"Cursor initialize;makeCursorsWithMask"

	self classPool associationsDo: [:var |
		var value hasMask
			ifFalse: [var value: var value withMask]] 