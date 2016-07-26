private
shrinkTo: newSize
	"if the used size is greater than newSize, then remove all the receiver's entries"
	
	used > newSize ifTrue:[self removeAll]