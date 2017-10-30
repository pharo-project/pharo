converting
inverted
	"Return new sort function with inverted direction then my, 
	if I'm currently ascending, I will return descending instance, and vice-versa."

	^self copy 
		direction: direction negated